trait(pick_trait(Condition, Name), Trait) :-
    valid_pick_trait(Condition, Name, Trait).

valid_pick_trait(Condition, Name, Selection) :-
    pick_trait(Condition, Name, Selection),
    \+ problem(pick_trait(Condition, Name), _).

% PC doesn't match the criteria to make a pick.
problem(pick_trait(Condition, Name), not_eligible) :-
    pick_trait(Condition, Name, _),
    \+ trait_options(Condition, Name, _, _).

% User didn't pick the correct number of options.
problem(pick_trait(Condition, Name), (should_pick(NOptions), have_picked(NSelected))) :-
    pick_trait(Condition, Name, Selection),
    feature_options(Condition, Name, NOptions, _),
    length(Selection, NSelected),
    NSelected \= NOptions.

% User picked a nonexistant option, without specific error message.
problem(pick_trait(Condition, Name), invalid_option(Selection)) :-
    pick_trait(Condition, Name, Selection),
    \+ trait_options(Condition, Name, _, Selection),
    \+ trait_bad_options(Condition, Name, Selection, _),
    trait_options(Condition, Name, _, _).

% User picked a nonexistant option, but for which we have a more specific error message.
problem(pick_trait(Condition, Name), bad_choice(Selection, ErrMsg)) :-
    pick_trait(Condition, Name, Selection),
    trait_bad_options(Condition, Name, Selection, ErrMsg).

% User picked multiple times for the same choice.
% TODO this generates a large number of duplicates, could become very very slow.
problem(pick_trait(Condition, Name), picked_more_than_once) :-
    trait_options(Condition, Name, _, _),
    findall(Pick, pick_trait(Condition, Name, Pick), [_,_|_]).
    
% TODO this generates a large number of duplicates, could become very very slow.
todo(no_option_picked(Condition, Name)) :-
    trait_options(Condition, Name, _, _),
    \+ pick_trait(Condition, Name, _).

