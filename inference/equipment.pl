:- multifile equipped/1.

equipped(_) :- false.

body_armor('studded leather', light, ac(12)).
body_armor(Armor+N, Weight, ac(AC)) :-
    body_armor(Armor, Weight, ac(BaseAC)),
    AC is BaseAC + N.

weapon_proficiency(Weapon) :-
    trait(weapon(Weapon)).
weapon_proficiency(Weapon) :-
    weapon(Weapon, WeaponClass, _, _, _),
    trait(weapon(WeaponClass)).
    
%! weapon(?Name, ?WeaponClass, ?Rangedness, ?DamageFormula, ?Notes)
weapon(rapier, martial, melee,
       [damage(piercing, 1 d 8)], [finesse]).
weapon('light crossbow', simple, ranged(feet(80) / feet(320)),
       [damage(piercing, 1 d 8)], [ammunition, loading, twohanded]).



% Possibilities
% "Melee weapons" can always melee and sometimes be thrown.
%   -> they always use STR (also when thrown), unless they are finesse weapons, in which case you can pick STR/DEX
% "Ranged weapons" cannot be used in melee
%   -> they always use DEX, unless they have the "thrown" property, in which case they use STR
%     -> UNLESS they also have finesse, in which case you can pick STR/DEX.

% A javelin is counted as a melee weapon with thrown property -> thrown simply modifies that it can also be used at range.
% A dart is counted as a ranged weapon with thrown property -> thrown here just indicates that it uses STR and not DEX (unless it's also finesse!).
