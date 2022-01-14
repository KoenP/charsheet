write_character_file :-
    name(Name),
    charname_to_filename(Name, FileName),
    open(FileName, write, Out),
    write_predicates(Out,
                     [name/1,
                      base_ability/2,
                      gain_level/3,
                      choice/3]),
    close(Out).

write_predicates(Out, List) :-
    forall(member(Pred,List), write_predicate(Out, Pred)).

write_predicate(Out, Pred/Arity) :-
    length(Args, Arity),
    Goal =.. [Pred|Args],
    forall(Goal, write_term(Out, Goal, [quoted(true), fullstop(true), nl(true)])).

load_character_file(CharName) :-
    charname_to_filename(CharName, FileName),
    load_files(FileName, []).

charname_to_filename(CharName, FileName) :-
    atomic_list_concat(['characters/', CharName, '.pl'], FileName).
