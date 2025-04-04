add_bonus(Bonus, Tm, Tm + Bonus).

sumall(X, Pred, Sum) :-
    findall(X, Pred, Xs),
    sum_list(Xs, Sum).

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

select_first_subterm(X, T1, Y, T2) :-
    select_subterm(X, T1, Y, T2),
    !.

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

fully_unwrap(Atom, Atom) :-
    atomic(Atom),
    !.
fully_unwrap(Compound, Kernel) :-
    Compound =.. [_, Subterm],
    fully_unwrap(Subterm, Kernel).

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

default_on_fail(_, Goal, X) :-
    call(Goal, X),
    !.
default_on_fail(Default, _, Default).

zip([], Ys, Ys).
zip(Xs, [], Xs).
zip([X|Xs], [Y|Ys], [X-Y|Zs]) :- zip(Xs, Ys, Zs).

enumerate(_, [], []) :- !.
enumerate(N, [X|Xs], [N-X|NXs]) :-
    !,
    M is N+1,
    enumerate(M, Xs, NXs).

simplify_product(0*_, 0).
simplify_product(X*0, 0) :- X \= 0.
simplify_product(1*X, X).
simplify_product(X*1, X) :- X \= 1.
simplify_product(X*Y, X*Y) :- X \= 1, Y \= 1.

odd(N) :- 1 is N mod 2.

extend_multimap(InitMap, _     , []    , InitMap).
extend_multimap(InitMap, GetKey, [X|Xs], Map    ) :-
    extend_multimap(InitMap, GetKey, Xs, Map1),
    call(GetKey, X, Key),
    add_to_multimap(Key, X, Map1, Map).



%multimap_from_list(GetKey, List, Map) :-
%    multimap_from_list_(GetKey, List, _{}, Map).
%
%multimap_from_list_(_, [], Acc, Acc).
%multimap_from_list_(GetKey, [X|Xs], Acc, Out) :-
%    call(GetKey, X, Key),
%    add_to_multimap(Key, X, Acc, Acc1),
%    multimap_from_list_(GetKey, Xs, Acc1, Out).
%

add_to_multimap(Key, X, In, Out) :-
    get_dict(Key, In, Entry),
    !,
    put_dict(Key, In, [X|Entry], Out).
add_to_multimap(Key, X, In, In.put(Key, [X])).

as_boolean(Goal, Bool) :-
    call(Goal) -> Bool = true ; Bool = false.

toggle(yes).

% Weird convenience predicate for infusions.
affirmative(X, yes, X).

id(X,X).
is_true(Goal, true) :- call(Goal), !.
is_true(_, false) :- !.

compose([], X, X).
compose([G|Gs], X, Z) :-
    call(G, X, Y),
    compose(Gs, Y, Z).

%! \/(Goal1, Goal2, Arg)
%
%  Second-order disjunction.
\/(Goal1, Goal2, Arg) :-
    call(Goal1, Arg) ; call(Goal2, Arg).

%! <<(G, F, X, Z)
%
%  Functional composition of two binary predicates F and G.
%  If we were to use functional notation for F and G: `Z = G(F(X))`.
<<(G, F, X, Z) :-
    call(F, X, Y),
    call(G, Y, Z).


%! /\(Goal1, Goal2, Arg)
%
%  Second-order conjunction.
/\(Goal1, Goal2, Arg) :-
    call(Goal1, Arg), call(Goal2, Arg).
