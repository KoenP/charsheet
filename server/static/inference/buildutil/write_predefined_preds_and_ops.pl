:- use_module(library(http/json)).
:- use_module(library(pldoc)).
:- use_module(library(yall)).
:- use_module(library(pairs)).
:- use_module(library(pure_input)).
:- use_module(library(http/html_write)).

main :-
    forall(current_predicate(P),
           (writeq(predefined_predicate(P)), write('.'), nl)),
    forall(current_op(Precedence, Type, Name),
           (writeq(predefined_op(Precedence, Type, Name)), write('.'), nl)),
    halt.
