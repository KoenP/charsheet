% TODO: delete valid_pick_trait, pick_trait, ...
%       replace trait_options everywhere

:- multifile
       % trait_options(Origin, UniqueName, OptionSpec)
       % Set of options to pick for a trait.
       % See options.pl to understand the OptionSpec parameter.
       trait_options/3,

       % problem/2 will automatically recognize when the user picked
       % an invalid trait option. But in some cases we may want to provide
       % a more specific error message for certain anticipated mistakes.
       % In that case, we assert
       % bad_trait_choice(Origin, Name, Choice, ErrMsg).
       bad_trait_choice/4,

       % The user uses this predicate to pick traits from a
       % trait_options/3 menu.
       choose_traits/3,

       % The player can use this to not pick a trait option so it won't show up
       % in their todo anymore (for example for the replace_spell
       % option for sorcerers).
       forego/2.

chosen_trait(Origin, Name, Trait) :-
    trait(choose_traits(Origin, Name), Trait).

trait(choose_traits(Origin, Name), Trait) :-
    valid_trait_choice(Origin, Name, Choice),
    member(Trait, Choice).

valid_trait_choice(Origin, Name, Choice) :-
    choose_traits(Origin, Name, Choice),
    \+ problem(choose_traits(Origin, Name, Choice), _).

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

% Generate a todo when the user has not yet picked any options for a trait they are eligible for.
todo(choose_traits(Origin, Name)) :-
    trait_options(Origin, Name, _),
    \+ forego(Origin, Name),
    \+ choose_traits(Origin, Name, _).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

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

