problems :-
    findall((P,E), problem(P,E), Problems),
    list_to_set(Problems, ProblemSet),
    maplist(writeln, ProblemSet).

todo :-
    findall(T, todo(T), Todos),
    list_to_set(Todos, TodoSet),
    maplist(writeln, TodoSet).

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
