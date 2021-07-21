:- multifile
       custom_display_rule//1.

display(Term, Display) :-
    phrase(display_term(Term), List),
    atomic_list_concat(List, Display).

display_term(Custom) --> custom_display_rule(Custom), !.
display_term(Compound) -->
    {\+ is_list(Compound), Compound =.. [Ftor|Tms], \+ member(Ftor, ['/', ':', 'd', '+']), Tms \= []},
    display_atom(Ftor),
    [' ('],
    display_terms(Tms),
    [')'].
display_term(X d Y) --> display_dice(X d Y).
display_term(X / Y) --> [X], ['/'], [Y].
display_term(X : Y) --> display_term(X), [':'], display_term(Y).
display_term(X + Y) --> display_term(X), ['+'], display_term(Y).
display_term(Number) --> {number(Number)}, [Number].
display_term(Atom) --> {atom(Atom)}, display_atom(Atom).
display_term(List) --> display_list(List).

display_atom(Atom) -->
    {atom_chars(Atom, Chars)},
    us_to_space(Chars).

display_terms([]) --> [].
display_terms([X]) --> display_term(X).
display_terms([X|Xs]) --> {Xs \= []}, display_term(X), [', '], display_terms(Xs).

display_list([]) --> [].
display_list([X]) --> display_term(X).
display_list([X|Xs]) --> {Xs \= []}, display_term(X), [', '], display_list(Xs).

display_dice_sum(Ds + K) --> {number(K)}, display_dice_sum(Ds), ['+'], [K].
display_dice_sum(Ds + D) --> display_dice_sum(Ds), ['+'], display_dice(D).
display_dice_sum(Ds) --> display_dice(Ds).
display_dice(N d X) --> seq([N, 'd', X]).

display_damage([R|Rs]) --> {Rs \= []}, display_damage_roll(R), ',', display_damage(Rs).
display_damage([R]) --> display_damage_roll(R).
display_damage([]) --> [].
display_damage_roll(Roll) -->
    {Roll =.. [Type, Dice]},
    display_dice_sum(Dice), [' '], [Type].

display_bonus(N) --> {N >= 0}, ['+'], [N].
display_bonus(N) --> {N < 0}, [N].
    
%! Replace underscores by spaces.
us_to_space([ X |Xs]) --> {X \= '_'}, [X], us_to_space(Xs).
us_to_space(['_'|Xs]) --> [' '], us_to_space(Xs).
us_to_space([]) --> [].
    

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%   
interleave([X|Xs], [Y|Ys]) --> [X], [Y], interleave(Xs, Ys).
interleave(Xs, []) --> {Xs \= []}, seq(Xs).
interleave([], Ys) --> seq(Ys).

sep(_, []) --> [].
sep(_, [X]) --> [X].
sep(Sep, [X|Xs]) --> {Xs \= []}, [X], seq(Sep), sep(Sep, Xs).

rep(_) --> [].
rep(X) --> [X], rep(X).

seq([]) --> [].
seq([E|Es]) --> [E], seq(Es).

maybe(_) --> [].
maybe(X) --> [X].
