:- multifile weapon/5.
:- multifile has/1.
:- dynamic has/1.

has(_) :- false.

expand_to_sum(Item    , Item + 0) :- Item \= _+_.
expand_to_sum(Item + N, Item + N).

is_shield(shield).
is_shield(Shield + _) :- is_shield(Shield).
is_shield(ShieldF) :- ShieldF =.. [shield|_].

body_armor('studded leather', light, ac(12)).
body_armor('half plate', medium, ac(15)).
body_armor('chain mail', heavy, ac(16)).
body_armor(Armor+N, Weight, ac(AC)) :-
    body_armor(Armor, Weight, ac(BaseAC)),
    AC is BaseAC + N.

weapon_proficiency(Weapon) :-
    trait(weapon(Weapon)).
weapon_proficiency(Weapon) :-
    weapon(Weapon, WeaponClass, _, _, _),
    trait(weapon(WeaponClass)).
    
%! weapon(?Name, ?WeaponClass, ?Rangedness, ?DamageFormula, ?Notes)
weapon(club, simple, melee,
       [damage(bludgeoning, 1 d 4)], [light]).
weapon(quarterstaff, simple, melee,
       [damage(bludgeoning, 1 d 6)], [versatile(1 d 8)]).
weapon(mace, simple, melee,
       [damage(bludgeoning, 1 d 6)], []).

weapon(greataxe, martial, melee,
       [damage(slashing, 1 d 12)], [heavy, twohanded]).
weapon(longsword, martial, melee,
       [damage(slashing, 1 d 8)], [versatile(1 d 10)]).
weapon(rapier, martial, melee,
       [damage(piercing, 1 d 8)], [finesse]).

weapon('light crossbow', simple, ranged(feet(80) / feet(320)),
       [damage(piercing, 1 d 8)], [ammunition, loading, twohanded]).
weapon(longbow, martial, ranged(feet(150) / feet(600)),
       [damage(piercing, 1 d 8)], [ammunition, heavy, twohanded]).

weapon(javelin, simple, melee,
       [damage(piercing, 1 d 6)], [thrown(feet(30) / feet(120))]).

% Possibilities
% "Melee weapons" can always melee and sometimes be thrown.
%   -> they always use STR (also when thrown), unless they are finesse weapons, in which case you can pick STR/DEX
% "Ranged weapons" cannot be used in melee
%   -> they always use DEX, unless they have the "thrown" property, in which case they use STR
%     -> UNLESS they also have finesse, in which case you can pick STR/DEX.

% A javelin is counted as a melee weapon with thrown property -> thrown simply modifies that it can also be used at range.
% A dart is counted as a ranged weapon with thrown property -> thrown here just indicates that it uses STR and not DEX (unless it's also finesse!).

weapon_onehanded(Weapon) :-
    weapon(Weapon, _, _, _, Notes),
    \+ member(twohanded, Notes).
weapon_melee(Weapon) :-
    weapon(Weapon, _, melee, _, _).
weapon_ranged(Weapon) :-
    weapon(Weapon, _, ranged(_), _, _).
