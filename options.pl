% TODO: delete valid_pick_trait, pick_trait, ...
%       replace trait_options everywhere

:- multifile
       % trait_options(Origin, UniqueName, OptionSpec)
       % Set of options to pick for a trait.
       % See options.pl to understand the OptionSpec parameter.
       trait_options/3,
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

       % The player can use this to not pick a trait option so it won't show up
       % in their todo anymore (for example for the replace_spell
       % option for sorcerers).
       forego/2.

:- table chosen_trait/3.
chosen_trait(Origin, Name, Trait) :-
    trait(choose_traits(Origin, Name), Trait).

gain_trait(Level, choose_traits(Origin, Name), Trait) :-
    %valid_trait_choice(Origin, Name, Choice),
    choose_traits(Origin, Name, Choice),
    trait_origin_level(Origin, Level),
    member(Option, Choice),
    %trait_options(Origin, Name, Wrap, _),
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

% User's choice doesn't match the spec, and we don't have a more specific error message.
% TODO: I think I can make more specific error messages than this.
problem(choose_traits(Origin, Name, Choice), doesnt_match_spec(Choice, Spec)) :-
    choose_traits(Origin, Name, Choice),
    is_list(Choice), % if it's not a list, we want another problem to fire
    trait_options(Origin, Name, Spec),
    \+ bad_trait_choice(Origin, Name, Choice, _),
    \+ choice_matches_spec(Choice, Spec, match).
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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

wrap(Functor, X, Term) :-
    Term =.. [Functor, X].


%trait(character_options(Origin, Name), Trait) :-
%    character_options(Origin, Name, Wrap, Spec),

%options(Origin, Name, id, Spec) :-
%    options(Origin, Name, Spec).
%id(X,X).
%    
%bad_pick(Origin, Name, PickedOptions) :-
%
%% User's choice does not match the specification, and we have a
%% case-specific error message for this case.
%problem(pick_options(Origin, Name, PickedOptions), bad_pick(Msg)) :-
%    pick_options(Origin, Name, PickedOptions),
%    bad_pick(Origin, Name, PickedOptions, Msg).
%
%% User has chosen an option that's not specified, and we don't have a more
%% specific error message.
%problem(pick_options(Origin, Name, PickedOptions), invalid_option(Trait)) :-
%    pick_options(Origin, Name, PickedOptions),
%    \+ bad_pick(Origin, Name, PickedOptions, _),
%    options(Origin, Name, Spec),
%    findall(Option, spec_option(Spec,Option), Options),
%    member(Trait, PickedOptions),
%    \+ member(Trait, Options).
%
%% User picked multiple times for the same choice.
%problem(pick_options(Origin, Name, PickedOptions), double_dip) :-
%    pick_options(Origin, Name, PickedOptions),
%    options(Origin, Name, _, _),
%    findall(_, pick_options(Origin, Name, _), [_,_|_]).
%
%% User picked the same trait multiple times within a single given choice.
%% We're assuming for now that this should never happen.
%problem(pick_options(Origin, Name, PickedOptions), picked_more_than_once(Trait)) :-
%    pick_options(Origin, Name, PickedOptions),
%    append(_, [Trait|Tail], PickedOptions),
%    append(_, [Trait|_], Tail).
%
%todo(pick_options(Origin, Name)) :-
%    options(Origin, Name, _, _),
%    \+ forego(Origin, Name),
%    \+ pick_options(Origin, Name, _).



% trait_options(level(4), test, 1 from [1 from Feats, 1 from [2 from AbiPlusOne, 1 from AbiPlusTwo]]) :-
%     findall(Ability+1, ability(Ability), AbiPlusOne),
%     findall(Ability+2, ability(Ability), AbiPlusTwo),
%     findall(feat(Feat), feat_option(Feat), Feats).
% bad_trait_choice(level(4), test, Asis, asis_sum_up_to(Sum)) :-
%     forall(member(X,Asis), X = _+_),
%     findall(X, member(_+X, Asis), Xs),
%     sumlist(Xs, Sum),
%     Sum \= 2.
% 
% choose_traits(level(4), test, [dex+3]).
% choose_traits(level(4), test, [dex+1,str+1]).
% choose_traits(level(4), test, [feat(alert)]).

