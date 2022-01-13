subterm_member(X, T) :-
    select_subterm(X, T, _, _).

unique_subterm_member(X, T) :-
    findall(Y, (Y=X, subterm_member(Y,T)), [X]).

select_subterm(X, X , Y, Y ).
select_subterm(X, T1, Y, T2) :-
    T1 =.. [Functor|Args],
    select(SubT, Args, NewSubT, NewArgs),
    select_subterm(X, SubT, Y, NewSubT),
    T2 =.. [Functor|NewArgs].

select_all_matching_members(_, [], _, []) :- !.
select_all_matching_members(X, [X|Xs], Y, [Y|Ys]) :-
    select_all_matching_members(X, Xs, Y, Ys).
select_all_matching_members(X, [X0|Xs], Y, [X0|Ys]) :-
    X \= X0,
    select_all_matching_members(X, Xs, Y, Ys).

map_matching_subterms(Goal, X, Y) :-
    call(Goal, X, Y), !.
map_matching_subterms(Goal, T1, T2) :-
    T1 =.. [Functor|Args1],
    !,
    maplist(map_matching_subterms(Goal), Args1, Args2),
    T2 =.. [Functor|Args2].
map_matching_subterms(_, X, X).

test(G, x) :- G =.. [g|_].

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

get_or_default(Dict, Field, _      , Dict.get(Field)) :- !.
get_or_default(_   , _    , Default, Default).

append_to_list(X, L1, L2) :-
    append(L1, [X], L2).

nonlist_to_singleton(X, [X]) :- \+ is_list(X).
nonlist_to_singleton(L,  L ) :-    is_list(L).

% [a:1, a:2, b:1] -> _{a:[1,2], b:[1]}
%multi_assoc_list_to_dict_of_dicts(Tag, Assocs) :-
%    dict_of_dicts_helper()
