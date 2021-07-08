:- op(400, xfx, d).

die_avg(X d Y, Avg) :- Avg is ceiling(X * (Y+1) / 2).

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

sum_to_list(X + Y, [Y|List]) :- sum_to_list(X, List).
sum_to_list(X, [X]) :- X \= _ + _.
    
list_to_sum([X], X).
list_to_sum([X|Xs], Sum + X) :- list_to_sum(Xs, Sum).

simplify_dice_sum(Dice, NewDs) :-
    sum_to_list(Dice, List),
    add_up_dice(List, SumList),
    list_to_sum(SumList, NewDs).

