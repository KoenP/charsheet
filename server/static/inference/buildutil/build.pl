:- [inference/main].
:- [inference/build/predefined_predicates_and_ops].

%read_file(Stream,[]) :-
%    at_end_of_stream(Stream).
%
%read_file(Stream,[X|L]) :-
%    \+ at_end_of_stream(Stream),
%    read_line_to_string(Stream,X),
%    read_file(Stream,L).
%main :-
%    open('inference/directives.pl', read, Str),
%    read_file(Str,Lines),
%    close(Str),
%    forall(member(Line, Lines), writeln(Line)),
%
%    forall((current_predicate(P), \+ predefined_predicate(P)),
%           listing(P)).

relevant_op(op(Precedence, Type, Name)) :-
    current_op(Precedence, Type, Name),
    \+ predefined_op(Precedence, Type, Name).

relevant_predicate(Pred) :-
    current_predicate(Pred),
    \+ predefined_predicate(Pred).

main :-
    forall(relevant_op(Op), (writeq(Op), write("."), nl)),
    forall(relevant_predicate(Pred), listing(Pred)).

