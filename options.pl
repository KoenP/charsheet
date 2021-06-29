
problem(pick(Condition, Name), no_such_option) :-
    pick(Condition, Name, _),
    \+ feature_options(Condition, Name, _, _).
problem(pick(Condition, Name), condition_unmatched) :-
    pick(Condition, Name, _),
    \+ matched(Condition).
problem(pick(Condition, Name), (should_pick(NOptions), have_picked(NSelected))) :-
    pick(Condition, Name, Selection),
    feature_options(Condition, Name, NOptions, _),
    length(Selection, NSelected),
    NSelected \= NOptions.
problem(pick(Condition, Name), invalid_option(Pick)) :-
    pick(Condition, Name, Selection),
    feature_options(Condition, Name, _, Options),
    member(Pick, Selection),
    \+ member(Pick, Options).

todo(no_option_picked(Condition, Name)) :-
    feature_options(Condition, Name, _, _),
    matched(Condition),
    \+ pick(Condition, Name, _).
todo(unsolved_problems(Problem, Error)) :-
    problem(Problem, Error).

valid_pick(Condition, Name, Selection) :-
    pick(Condition, Name, Selection),
    \+ problem(pick(Condition, Name), _).
