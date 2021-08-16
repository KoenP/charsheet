% TODO: delete valid_pick_trait, pick_trait, ...
%       replace trait_options everywhere

:- multifile
       % trait_options(Origin, UniqueName, OptionSpec)
       % Set of options to pick for a trait.
       % See options.pl to understand the OptionSpec parameter.
       trait_options/3,
       trait_replacement_options/4,
       wrap_trait_option/4,

       % problem/2 will automatically recognize when the user picked
       % an invalid trait option. But in some cases we may want to provide
       % a more specific error message for certain anticipated mistakes.
       % In that case, we assert
       % bad_trait_choice(Origin, Name, Choice, ErrMsg).
       bad_trait_choice/4,

       % The user uses this predicate to pick traits from a
       % trait_options/3 menu.
       choose_traits/3,
       choose_trait/3,

       % TODO
       replace_traits/4,

       % The player can use this to not pick a trait option so it won't show up
       % in their todo anymore (for example for the replace_spell
       % option for sorcerers).
       forego/2.

:- table chosen_trait/3.
chosen_trait(Origin, Name, Trait) :-
    trait(choose_traits(Origin, Name), Trait).

gain_trait(Level, choose_traits(Origin, Name), Trait) :-
    choose_traits(Origin, Name, Choice),
    trait_origin_level(Origin, Level),
    member(Option, Choice),
    wrapped_trait_option(Origin, Name, Option, Trait).
gain_trait(Level, replace_traits(Origin, Name, Old, New), Trait) :-
    replace_traits(Origin, Name, Old, New),
    trait_origin_level(Origin, Level),
    member(Option, New),
    wrapped_trait_option(Origin, Name, Option, Trait).
lose_trait(Level, replace_traits(Origin, Name, Old, New), Trait) :-
    replace_traits(Origin, Name, Old, New),
    trait_origin_level(Origin, Level),
    member(Option, New),
    wrapped_trait_option(Origin, Name, Option, Trait).

wrapped_trait_option(Origin, Name, Option, Trait) :-
    wrap_trait_option(Origin, Name, Option, Trait),
    !.
wrapped_trait_option(_, _, Trait, Trait).

%valid_trait_choice(Origin, Name, Choice) :-
%    choose_traits(Origin, Name, Choice),
%    \+ problem(choose_traits(Origin, Name, Choice), _).

% choose_trait lets the user pick traits one by one instead of with a list
choose_traits(Origin, Name, Choice) :-
    choose_trait(Origin, Name, _),
    findall(Option, choose_trait(Origin, Name, Option), Choice),
    Choice \= [].

% spec_option(Spec, Option): Option is one of the options available in the Spec.
spec_option(_ from List, Option) :-
    member(X, List),
    spec_option(X, Option).
spec_option(Trait, Trait) :-
    Trait \= _ from _.

% PC is not eligible to make the given choice.
problem(choose_traits(Origin, Name, Choice), not_eligible(Origin, Name)) :-
    choose_traits(Origin, Name, Choice),
    \+ trait_options(Origin, Name, _).

% User's trait selection is not a list.
problem(choose_traits(Origin, Name, Choice), not_a_list(Choice)) :-
    choose_traits(Origin, Name, Choice),
    \+ is_list(Choice).
problem(replace_traits(Origin, Name, Old, New), not_a_list(X)) :-
    replace_traits(Origin, Name, Old, New),
    (\+ is_list(Old) -> X = Old; \+ is_list(New) -> X = New).

% User's choice doesn't match the spec, and we don't have a more specific error message.
% TODO: I think I can make more specific error messages than this.
problem(choose_traits(Origin, Name, Choice), doesnt_match_spec(Choice, Spec)) :-
    choose_traits(Origin, Name, Choice),
    is_list(Choice), % if it's not a list, we want another problem to fire
    trait_options(Origin, Name, Spec),
    \+ bad_trait_choice(Origin, Name, Choice, _),
    \+ choice_matches_spec(Choice, Spec, match).
problem(replace_traits(Origin, Name, Old, New), doesnt_match_spec(Old, Spec)) :-
    replace_traits(Origin, Name, Old, New),
    is_list(Old),
    trait_replacement_options(Origin, Name, OldSpec, _),
    \+ choice_matches_spec(Old, OldSpec, match).
%problem(replace_traits(Origin, Name, Old, New), was_not_a_trait(Old, Spec)) :-
%    replace_traits(Origin, Name, Old, New),
%    is_list(Old),

    
choice_matches_spec(Choice, N from Spec, Result) :-
    findall(X, (member(SubSpec,Spec), choice_matches_spec(Choice,SubSpec,X)), Results),
    fold_matches(N, Results, Result).
choice_matches_spec(Choice, Trait, match) :-
    Trait \= _ from _,
    member(Trait, Choice).
choice_matches_spec(Choice, Trait, no_match) :-
    Trait \= _ from _,
    \+ member(Trait, Choice).
fold_matches(_, Rs, bad) :-
    member(bad, Rs).
fold_matches(N, Rs, Result) :-
    \+ member(bad, Rs),
    count(match, Rs, M),
    ( N = M           -> Result = match
    ; M = 0           -> Result = no_match
    ; (M > 0, M \= N) -> Result = bad).
count(X, L, N) :-
    findall(_, member(X,L), Xs),
    length(Xs, N).
    
% User's choice does not match the specification, and we have a
% case-specific error message for this case.
problem(choose_traits(Origin, Name, Choice), bad_choice(Msg)) :-
    choose_traits(Origin, Name, Choice),
    bad_trait_choice(Origin, Name, Choice, Msg).

% User has chosen an option that's not specified, and we don't have a more
% specific error message.
problem(choose_traits(Origin, Name, Choice), invalid_option(Trait)) :-
    choose_traits(Origin, Name, Choice),
    \+ bad_trait_choice(Origin, Name, Choice, _),
    trait_options(Origin, Name, Spec),
    findall(Option, spec_option(Spec,Option), Options),
    member(Trait, Choice),
    \+ member(Trait, Options).

% User picked multiple times for the same choice.
problem(choose_traits(Origin, Name, Choice), double_dip) :-
    choose_traits(Origin, Name, Choice),
    trait_options(Origin, Name, _),
    findall(_, choose_traits(Origin, Name, _), [_,_|_]).

% User picked the same trait multiple times within a single given choice.
% We're assuming for now that this should never happen.
problem(choose_traits(Origin, Name, Choice), picked_more_than_once(Trait)) :-
    choose_traits(Origin, Name, Choice),
    append(_, [Trait|Tail], Choice),
    append(_, [Trait|_], Tail).

% User declared they will forego a trait option, but then they pick anyway.
problem(choose_traits(Origin, Name, Choice), chosen_traits_for_foregone_choice) :-
    choose_traits(Origin, Name, Choice),
    forego(Origin, Name).

% User foregoes a trait option they're not eligible for.
problem(forego(Origin, Name), not_eligible(Origin, Name)) :-
    forego(Origin, Name),
    \+ trait_options(Origin, Name, _).

% Generate a todo when the user has not yet picked any options for a trait they are eligible for.
todo(choose_traits(Origin, Name)) :-
    trait_options(Origin, Name, _),
    \+ forego(Origin, Name),
    \+ choose_traits(Origin, Name, _).

