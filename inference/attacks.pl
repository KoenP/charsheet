:- multifile attack/5.

%! attack(?Name, ?Range, ?ToHitOrDC, ?DamageFormula, ?Notes)
attack(Cantrip, Range, to_hit(ToHit), [DamageDice], []) :-
    known_spell_to_hit(Origin, Cantrip, ToHit),
    known_spell_data(Origin, Cantrip, Data),
    Data.level = 0,
    Data.range = Range,
    unique_subterm_member(spell_attack_roll(_):DamageDice, Data.effects).
attack(Cantrip, Range, saving_throw(DC, Abi), [DamageDice], Notes) :-
    known_spell_saving_throw(Origin, Cantrip, DC, Abi),
    known_spell_data(Origin, Cantrip, Data),
    Data.level = 0,
    Data.range = Range,
    unique_subterm_member(saving_throw(_):Effect, Data.effects),
    ( (Effect = damage(_,_), Effect = DamageDice, Notes = [])
    ; (Effect = (DamageDice else Alt),
       DamageDice = damage(_,_)),
       Notes = [Str],
       format(string(Str), "on save: ~w", Alt)).
attack(MeleeWeapon, MeleeRange, to_hit(ToHit), FinalDamageRolls, Notes) :-
    equipped(MeleeWeapon),
    weapon(MeleeWeapon, _, melee, DamageRolls, Notes),
    weapon_attack_modifier(melee, Notes, _, Mod),
    weapon_proficiency_bonus(MeleeWeapon, ProfBon),
    ToHit is Mod + ProfBon,
    add_bonus_to_first_die(Mod, DamageRolls, FinalDamageRolls),
    weapon_melee_range(MeleeWeapon, MeleeRange).
attack(RangedWeapon, Range, to_hit(ToHit), FinalDamageRolls, Notes) :-
    equipped(RangedWeapon),
    weapon(RangedWeapon, _, ranged(Range), DamageRolls, Notes),
    weapon_attack_modifier(ranged(Range), Notes, _, Mod),
    weapon_proficiency_bonus(RangedWeapon, ProfBon),
    ToHit is Mod + ProfBon,
    add_bonus_to_first_die(Mod, DamageRolls, FinalDamageRolls).

add_bonus_to_first_die(Bonus, [damage(Type,Roll)|Rolls], [damage(Type,NewRoll)|Rolls]) :-
    simplify_dice_sum(Roll+Bonus, NewRoll).
    

weapon_melee_range(Weapon, feet(MeleeRange)) :-
    weapon(Weapon,_,_,_,Notes),
    findall(Ft, (member(reach(feet(Ft)),Notes); bonus(reach+feet(Ft))), Fts),
    sum_list(Fts, Bonus),
    MeleeRange is 5 + Bonus.

weapon_attack_modifier(_, Notes, Abi, Mod) :-
    member(finesse, Notes),
    highest_ability_from([str,dex], Abi),
    ability_mod(Abi, Mod).
weapon_attack_modifier(melee, Notes, str, Mod) :-
    \+ member(finesse, Notes),
    ability_mod(str, Mod).
weapon_attack_modifier(ranged(_), Notes, Abi, Mod) :-
    \+ member(finesse, Notes),
    (member(thrown, Notes)
     -> Abi = str, ability_mod(str, Mod)
     ;  Abi = dex, ability_mod(dex, Mod)).
    
weapon_proficiency_bonus(Weapon, ProfBon) :-
    weapon_proficiency(Weapon),
    !,
    proficiency_bonus(ProfBon).
weapon_proficiency_bonus(_, 0).
