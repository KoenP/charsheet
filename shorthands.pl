problems :-
    setof(P:E, problem(P,E), Problems),
    maplist(writeln, Problems).

todo :-
    setof(T, todo(T), Todos),
    maplist(writeln, Todos).

scores :-
    findall(Ability:Score, ability(Ability, Score), Scores),
    maplist(writeln, Scores).

ac :- ac(AC), writeln(AC).

info(X) :- (X ?= D), writeln(D).

match(X, X).
match(X, Y) :-
    Y =.. L,
    L = [_,_|_],
    member(Z, L),
    match(X, Z).
search(X) :-
    (Y ?= D),
    match(X, Y),
    write("Found: "),
    writeln(Y),
    writeln(D).
