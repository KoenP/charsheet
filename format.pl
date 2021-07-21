:- multifile
       custom_format//1.

format(Term, Format) :-
    phrase(format_term(Term), List),
    atomics_to_string(List, Format).

format_term(Custom) --> custom_format(Custom), !.
format_term(Compound) -->
    {\+ is_list(Compound), Compound =.. [Ftor|Tms], \+ member(Ftor, ['/', ':', 'd', '+']), Tms \= []},
    format_atom(Ftor),
    [' ('],
    format_terms(Tms),
    [')'].
format_term(X d Y) --> format_dice(X d Y).
format_term(X / Y) --> [X], ['/'], [Y].
format_term(X : Y) --> format_term(X), [':'], format_term(Y).
format_term(X + Y) --> format_term(X), ['+'], format_term(Y).
format_term(Number) --> {number(Number)}, [Number].
format_term(Atom) --> {atom(Atom)}, format_atom(Atom).
format_term(List) --> format_list(List).

format_atom(Atom) -->
    {atom_chars(Atom, Chars)},
    us_to_space(Chars).

format_terms([]) --> [].
format_terms([X]) --> format_term(X).
format_terms([X|Xs]) --> {Xs \= []}, format_term(X), [', '], format_terms(Xs).

format_list([]) --> [].
format_list([X]) --> format_term(X).
format_list([X|Xs]) --> {Xs \= []}, format_term(X), [', '], format_list(Xs).

format_list_flat([]) --> [].
format_list_flat([X]) --> [X].
format_list_flat([X|Xs]) --> {Xs \= []}, [X], [', '], format_list_flat(Xs).

format_dice_sum(Ds + K) --> {number(K)}, format_dice_sum(Ds), ['+'], [K].
format_dice_sum(Ds + D) --> format_dice_sum(Ds), ['+'], format_dice(D).
format_dice_sum(Ds) --> format_dice(Ds).
format_dice(N d X) --> seq([N, 'd', X]).

format_damage([R|Rs]) --> {Rs \= []}, format_damage_roll(R), ',', format_damage(Rs).
format_damage([R]) --> format_damage_roll(R).
format_damage([]) --> [].
format_damage_roll(Roll) -->
    {Roll =.. [Type, Dice]},
    format_dice_sum(Dice), [' '], [Type].

format_bonus(N) --> {N >= 0}, ['+'], [N].
format_bonus(N) --> {N < 0}, [N].

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
