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

describe(X) :- describe(X,D), writeln(D).

match(X, Y) :-
    term_string(X, XString),
    string_to_list(XString, XList),
    term_string(Y, YString),
    string_to_list(YString, YList),
    append([_, XList, _], YList).
search(X) :-
    describe(Y, D),
    match(X, Y),
    write("Found: "),
    writeln(Y),
    writeln(D).
