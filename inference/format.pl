:- multifile custom_format//1.
:- multifile special_plural//1.

fmt(Spec, Out) :-
    phrase(Spec, Phrase),
    atomics_to_string(Phrase, Out).

format_term(Var) --> {var(Var), !}, [''].
format_term(T) --> custom_format(T), {!}.
format_term(X d Y) --> format_dice(X d Y), {!}.
format_term(1 / 2) --> {!}, ["½"].
format_term(X / Y) --> {!}, format_term(X), ['/'], format_term(Y).
format_term(X : Y) --> {!}, format_term(X), [':'], format_term(Y).
format_term(X + Y) --> {!}, format_term(X), ['+'], format_term(Y).
format_term(X = Y) --> {!}, format_term(X), ['='], format_term(Y).
format_term(X -> Y) --> {!}, format_term(X), ['→'], format_term(Y).
format_term(X pct) --> {!}, format_term(X), ['%'].
format_term(X upto Y) --> {!}, format_term(X), [' up to '], format_term(Y).
format_term(feet(Ft)) --> format_number(Ft), {!}, [' ft'].
format_term(hours(1)) --> {!}, ['1 hour'].
format_term(hours(N)) --> {!}, format_term(N), [' hours'].
format_term(cr(X)) --> {!}, ['CR '], format_term(X).
format_term(Number) --> format_number(Number), {!}.
format_term(Atom) --> {atom(Atom), !}, format_atom(Atom).
format_term(String) --> {string(String), !}, [String].
format_term(List) --> {is_list(List), !}, format_list(List).
format_term(Compound) -->
    {Compound =.. [Ftor|Tms], Tms \= []},
    %{\+ is_list(Compound), Compound =.. [Ftor|Tms], \+ member(Ftor, ['/', ':', 'd', '+', pct, cr, upto]), Tms \= []},
    format_atom(Ftor),
    [' ('],
    format_terms(Tms),
    [')'].

format_number(Number) --> {number(Number), !, number_chars(Number, Chars)}, seq(Chars). 

format_atom(Atom) -->
    {atom_chars(Atom, Chars)},
    us_to_space(Chars).

format_terms([]) --> [].
format_terms([X]) --> format_term(X).
format_terms([X|Xs]) --> {Xs \= []}, format_term(X), [', '], format_terms(Xs).

format_list_empty_as_dash([]) --> ['-'].
format_list_empty_as_dash(L) --> {L = [_|_]}, format_list(L).

format_list([]) --> {!}, [].
format_list([X]) --> {!}, format_term(X).
format_list([X|Xs]) --> {Xs \= [], !}, format_term(X), [', '], format_list(Xs).

format_list_flat([]) --> [].
format_list_flat([X]) --> [X].
format_list_flat([X|Xs]) --> {Xs \= []}, [X], [', '], format_list_flat(Xs).

format_damage([]) --> {!}, [].
format_damage([R]) --> {!}, format_damage_roll(R).
format_damage([R|Rs]) --> {Rs \= []}, format_damage_roll(R), ['+'], format_damage(Rs).
format_damage_roll(damage(Type,Dice)) -->
    format_dice_sum(Dice), [' '], [Type].

format_range(feet(X)) --> {!}, [X], [" ft"].
format_range(miles(X)) --> {!}, [X], [" mi"].
format_range(Short/Long) --> {!}, format_range(Short), ['/'], format_range(Long).
format_range(X) --> [X].


format_dice_sum(Ds + K) --> {number(K), K \= 0, !}, format_dice_sum(Ds), ['+'], [K].
format_dice_sum(Ds + in_parens(Ds2)) -->
    format_dice_sum(Ds),
    ['(+'],
    format_dice_sum(Ds2),
    [')'].
format_dice_sum(Ds + 0) --> {!}, format_dice_sum(Ds).
format_dice_sum(Ds + D) --> {!}, format_dice_sum(Ds), ['+'], format_dice(D).
format_dice_sum(Ds) --> format_dice(Ds).
format_dice(N d X) --> seq([N, 'd', X]).
format_dice(N) --> {number(N)}, [N].

format_bonus(N) --> {N >= 0}, ['+'], [N].
format_bonus(N) --> {N < 0}, [N].
format_bonus_str(N, Str, Tail) :-
    format_bonus(N, Fmt, Tail),
    atomic_list_concat(Fmt, Str).

format_measure(Measure) -->
    {Measure =.. [Unit, Magnitude]},
    [Magnitude],
    [" "],
    [Unit].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Format effects.
format_effects([]) --> [].
format_effects([E|Es]) -->
    format_effect(E),
    ["; "],
    format_effects(Es).

format_effect(spell_attack_roll(_):Effects) -->
    {is_list(Effects), !},
    ["on hit: "],
    format_effects(Effects).
format_effect(spell_attack_roll(_):Effect) -->
    format_effect(Effect),
    [" on hit"].
format_effect(custom_attack_roll(ToHit):Effect) -->
    ["make attack roll with "], format_bonus(ToHit), [" to hit"],
    format_effect(Effect).
format_effect(in(Area):Effect) -->
    ["in "],
    format_area(Area),
    [": "],
    format_effect(Effect),
    {!}.
format_effect(1*Es) --> {!}, format_effect(Es).
format_effect(N*Es) -->
    {!},
    [N],
    [" times "],
    format_effect(Es).
format_effect(push(Dist)) -->
    {!},
    ["push "],
    format_term(Dist).
format_effect(saving_throw(dc(Abi,DC)):(E1 else E2)) -->
    {!},
    [Abi],
    [" saving throw (DC "],
    format_number(DC),
    [") → "],
    format_effect(E1),
    [" on fail, else "],
    format_effect(E2).
format_effect(saving_throw(Abi):(E1 else E2)) -->
    {!},
    ["saving throw ("],
    [Abi],
    [") → "],
    format_effect(E1),
    [" on fail, else "],
    format_effect(E2).
format_effect(saving_throw(Abi):Effect) -->
    {!},
    ["saving throw ("],
    [Abi],
    [") -> "],
    format_effect(Effect),
    [" on fail"].
format_effect(damage(Type,Roll)) -->
    {!},
    ["deal "],
    format_damage_roll(damage(Type,Roll)),
    [" damage"].
format_effect(List) -->
    {is_list(List), !},
    format_effects(List).
format_effect(E) -->
    format_term(E).

format_area(N ft Shape) --> [N], [" ft "], [Shape].
format_area(N by M ft Shape) --> [N], ["×"], [M], [" ft "], [Shape].

format_ref(srd(Page)) --> format_term(phb(Page)).
format_ref(T) --> format_term(T).

% Replace underscores by spaces.
us_to_space([ X |Xs]) --> {X \= '_'}, [X], us_to_space(Xs).
us_to_space(['_'|Xs]) --> [' '], us_to_space(Xs).
us_to_space([]) --> [].

interleave([X|Xs], [Y|Ys]) --> [X], [Y], interleave(Xs, Ys).
interleave(Xs, []) --> {Xs \= []}, seq(Xs).
interleave([], Ys) --> seq(Ys).

emph(Ph) --> ["**"], phrase(Ph), ["**"].

unwords(Words) --> sep(" ", Words).
unlines(Lines) --> sep("\n", Lines).

sep(_  , []       ) --> {!}, [].
sep(_  , [Ph]     ) --> {!}, phrase(Ph).
sep(Sep, [Ph|Phs] ) --> phrase(Ph), [Sep], sep(Sep, Phs).

rep(_) --> [].
rep(X) --> [X], rep(X).

seq([]) --> [].
seq([E|Es]) --> [E], seq(Es).

seq_atom(Atom) --> {atom_chars(Atom, Chars)}, seq(Chars).

maybe(_) --> [].
maybe(X) --> [X].

to_plural(Noun)   --> special_plural(Noun), {!}.
to_plural(Noun)   --> {atom(Noun)}, format_atom(Noun), ['s'].
to_plural(GiveUp) --> [GiveUp].
