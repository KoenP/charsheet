:- multifile multiclass_trait/2, multiclass_trait_source/2.

% Channel divinity has specific multiclassing rules.
res('channel divinity', Uses) :-
    findall(N, bonus(channel_divinity_uses(N)), Ns),
    max_member(Uses, Ns).
restore_res('short rest', 'channel divinity', 'full restore').

custom_format(channel_divinity(CD)) -->
    ["Channel Divinity: "], format_term(CD).

%! multiclass_trait(?Origin, ?Trait)
%
%  A trait that can originate from multiple classes at once,
%  for which the rules define a specific way to handle overlap or conflicts.
multiclass_trait(Origin, Trait) :-
    multiclass_trait_source(Origin, Trait),
    call(Origin).

%! multiclass_trait_source(?Origin, ?Trait)
%
%  If `call(Origin)` is true, results in a `multiclass_trait(Origin, Traint)`.
multiclass_trait_source(_,_) :- false.

% Extra attack (fighter, barbarian, monk, ...).
trait(Origin, extra_attack(N)) :-
    findall(O-N, multiclass_trait(O, extra_attack(N)), Origins),
    sort(2, @>=, Origins, [Origin-N|_]).
bonus(trait(extra_attack(N)), add_weapon_note(_, Note)) :-
    trait(extra_attack(N)),
    M is N+1,
    atomics_to_string(["attack ", M, "x"], Note).
custom_format(extra_attack(1)) --> ["one extra attack"].
custom_format(extra_attack(N)) --> {N \= 1}, [N], [" extra attacks"].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

extra_attack(_) ?= "You can attack more than once whenever you take the Attack action on your turn.".
