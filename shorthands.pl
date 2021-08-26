problems :-
    setof(P:E, problem(P,E), Problems),
    maplist(writeln, Problems).

warn_if_problems :-
    problem(_, _),
    !,
    writeln("WARNING: your character sheet has some problems").
warn_if_problems :- true.

todo :-
    setof(T, todo(T), Todos),
    maplist(writeln, Todos).

fishy :-
    setof(A:B, fishy(A,B), L),
    maplist(writeln, L).

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

showopts(Origin, Name) :-
    trait_options(Origin, Name, Spec),
    findall(O, spec_option(Spec, O), Os),
    maplist(writeln, Os).
