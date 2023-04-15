% TODO: documentation and cleanup
:- op(400, xfx, d).

%! roll_avg(?Roll, -Avg:int)
%
%  The average value of a die, rounded up.
roll_avg(X d Y, Avg) :- Avg is ceiling(X * (Y+1) / 2).

%! roll_max(?Roll, -Max:int)
%
%  The maximum value of a die roll.
roll_max(X d Y, Max) :- Max is X*Y.

%! simplify_dice_sum(?Sum, ?Simplified)
%
%  Simplify a sum of dice. For example, `1 d 4 + 3 + 2 d 4 + 1` is
%  simplified to `3 d 4 + 4`.
%  Makes sure the dice are ordered in descending number of eyes, with
%  the constant term at the very end.
simplify_dice_sum(Sum, Simplified) :-
    sum_to_list(Sum, List),
    add_up_dice(List, SumList),
    sort(2, @<, SumList, SumListSorted),
    list_to_sum(SumListSorted, Simplified).

%! normalize_dice_formula(?Sum, ?Normalized)
%
%  Normalized is a sum of dice equivalent to Sum, but always ending in
%  `+0`. If Sum already ends in `+0`, then Normalized = Sum,
%  otherwise, Normalized is the same Sum but with an addition `+0`
%  term.
normalize_dice_formula(Dice + N, Dice + N) :-
    number(N),
    N \= 0,
    !.
normalize_dice_formula(Dice , Dice + 0).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Internal representations for this file.
sum_to_list(X    , [DX]      ) :-
    X \= _ + _,
    normalize_dice_sum_term(X,DX).
sum_to_list(X + Y, [DY|List]) :-
    normalize_dice_sum_term(Y,DY),
    sum_to_list(X, List).

normalize_dice_sum_term(N d X, N d X) :- X \= 1.
normalize_dice_sum_term(N    , N d 1) :- number(N).
add_up_dice([0 d _ | Ds], Dice) :-
    !,
    add_up_dice(Ds, Dice).
add_up_dice([D|Ds], Dice) :-
    add_up_dice(Ds, RestDice),
    add_die_to_list(D, RestDice, Dice).
add_up_dice([], []).
add_die_to_list(N d X, OldList, NewList) :-
    append(L1, [M d X|L2], OldList),
    O is N + M,
    append(L1, [O d X|L2], NewList).
add_die_to_list(N d X, List, [N d X | List]) :-
    \+ member(_ d X, List).

list_to_sum([DX], X) :-
    normalize_dice_sum_term(X, DX).
list_to_sum([DX|Xs], Sum + X) :-
    list_to_sum(Xs, Sum),
    normalize_dice_sum_term(X, DX).

strictly_worse(N d M1, N d M2) :- M1 < M2.
strictly_worse(N1 d M, N2 d M) :- N1 < N2.
