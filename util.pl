subterm_member(X, T) :-
    select_subterm(X, T, _, _).

select_subterm(X, X , Y, Y ).
select_subterm(X, T1, Y, T2) :-
    T1 =.. [Functor|Args],
    select(SubT, Args, NewSubT, NewArgs),
    select_subterm(X, SubT, Y, NewSubT),
    T2 =.. [Functor|NewArgs].

sequence([], X, X).
sequence([Pred|Preds], X, Z) :-
    call(Pred, X, Y),
    sequence(Preds, Y, Z).
    
%! ordered_lookup_largest_leq(+KVTable, +Key, -ValueFound)
% 
%  Look up the value associated with the largest key smaller than or
%  equal to the parameter key, in the provided table.
ordered_lookup_largest_leq([Key -> Value|Table], KeyToLookup, ValueFound) :-
    Key =< KeyToLookup,
    ( ordered_lookup_largest_leq(Table, KeyToLookup, ValueFound), !
    ; ValueFound = Value
    ).
