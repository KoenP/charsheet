

item(padded_armor, value(5), weight(8)).

body_armor(padded, light, ac(11), [disadvantage(stealth)]).
body_armor(leather, light, ac(11), []).
body_armor(halfplate, medium, ac(15), [disadvantage(stealth)]).

% Every weapon in inventory is listed as an attack option.
% TODO mostly duplicated clauses
attack(Weapon, Range, DamageType, ToHit, DamageDice, Notes) :-
    have(Weapon),
    plus_zero(Weapon, BaseWeapon + Enchantment),
    weapon(BaseWeapon, Range, DamageType, BaseDamageDice, Notes),
    weapon_to_hit(BaseWeapon + Enchantment, Ability, ToHit),
    ability_mod(Ability, AbiMod),
    weapon_damage_dice(BaseDamageDice + Enchantment, AbiMod, DamageDicePlusZero),
    plus_zero(DamageDice, DamageDicePlusZero).
attack(Weapon:'two handed', Range, DamageType, ToHit, DamageDice, Notes) :-
    have(Weapon),
    plus_zero(Weapon, BaseWeapon + Enchantment),
    weapon(BaseWeapon, Range, DamageType, _, Notes),
    member(versatile(VersatileDice), Notes),
    weapon_to_hit(BaseWeapon + Enchantment, Ability, ToHit),
    ability_mod(Ability, AbiMod),
    weapon_damage_dice(VersatileDice + Enchantment, AbiMod, DamageDicePlusZero),
    plus_zero(DamageDice, DamageDicePlusZero).
weapon_damage_dice(X d Y + I, AbiMod, X d Y + J) :-
    J is I + AbiMod.
weapon_to_hit(BaseWeapon + Enchantment, Ability, Modifier) :-
    trait(weapon(BaseWeapon)),
    !,
    proficiency_bonus(ProfBon),
    best_weapon_ability_modifier(BaseWeapon, Ability, Mod),
    Modifier is Mod + ProfBon + Enchantment.
weapon_to_hit(BaseWeapon + Enchantment, Ability, ToHit) :-
    best_weapon_ability_modifier(BaseWeapon, Ability, Modifier),
    ToHit is Modifier + Enchantment.
best_weapon_ability_modifier(Weapon, Ability, Mod) :-
    weapon(Weapon, _, _, _, _),
    findall(Abil:X, weapon_ability_modifier(Weapon, Abil, X), Mods),
    max_member_by(snd, Ability:Mod, Mods).
weapon_ability_modifier(Weapon, str, Mod) :-
    weapon(Weapon, melee, _, _, _),
    ability_mod(str, Mod).
weapon_ability_modifier(Weapon, dex, Mod) :-
    weapon(Weapon, melee, _, _, Notes),
    member(finesse, Notes),
    ability_mod(dex, Mod).
weapon_ability_modifier(Weapon, dex, Mod) :-
    weapon(Weapon, Range, _, _, _),
    Range \= melee,
    ability_mod(dex, Mod).

snd(_:Y, Y).
max_member_by(_, X, [X]).
max_member_by(Goal, X, [X|Ys]) :-
    max_member_by(Goal, Y, Ys),
    call(Goal, X, X1),
    call(Goal, Y, Y1),
    X1 > Y1.
max_member_by(Goal, Y, [X|Ys]) :-
    max_member_by(Goal, Y, Ys),
    call(Goal, X, X1),
    call(Goal, Y, Y1),
    Y1 > X1.

plus_zero(Weapon+Enchantment, Weapon+Enchantment) :-
    Enchantment \= 0.
plus_zero(Weapon, Weapon+0) :-
    Weapon \= _ + _.

% Weapon stats.
weapon(scimitar, melee, slashing, 1 d 6, [finesse, light]).
weapon(quarterstaff, melee, bludgeoning, 1 d 6, [versatile(1 d 8)]).
weapon(battleaxe, melee, slashing, 1 d 8, [versatile(1 d 10)]).
