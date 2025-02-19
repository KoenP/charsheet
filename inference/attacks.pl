:- multifile attack/5, attack_variant/5, add_weapon_note/2, counts_as_unarmed/1, has_natural_weapon/1, has_unarmed/1.

%! attack(?Name, ?Range, ?ToHitOrDC, ?DamageFormula, ?Notes)
%
%  Stats for an attack (typically weapon or cantrip) that is available
%  to the character.
%  For weapons (or weapon variants), this means the weapon has to be
%  in the character's posession (see has/1)
%  For cantrips, the spell has to be known (see known_spell/2).
attack(Weapon, Range, ToHit, DamageRolls, Notes) :-
    (has(Weapon) ; has_unarmed(Weapon)),
    weapon_attack(Weapon, Range, ToHit, DamageRolls, Notes).

attack(Cantrip, Range, to_hit(ToHit), [DamageDice], Notes) :-
    known_spell_to_hit(Origin, Cantrip, ToHit),
    known_spell_data(Origin, Cantrip, Data),
    Data.level = 0,
    Data.range = Range,
    unique_subterm_member(spell_attack_roll(_):DamageDice, Data.effects),
    findall(Note, cantrip_attack_note(Data, Note), Notes).

attack(Cantrip, Range, saving_throw(DC, Abi), [DamageDice], Notes) :-
    known_spell_saving_throw(Origin, Cantrip, DC, Abi),
    known_spell_data(Origin, Cantrip, Data),
    Data.level = 0,
    Data.range = Range,
    unique_subterm_member(saving_throw(_):Effect, Data.effects),
    ( (Effect = damage(_,_), Effect = DamageDice)
    ; (Effect = (DamageDice else _), DamageDice = damage(_,_))
    ),
    findall(Note, cantrip_attack_note(Data, Note), Notes).

cantrip_attack_note(CantripData, Note) :-
    member(Note, CantripData.effects),
    string(Note).
cantrip_attack_note(CantripData, Note) :-
    unique_subterm_member(N*(spell_attack_roll(_):_), CantripData.effects),
    format(string(Note), "attack ~wx", N).
cantrip_attack_note(CantripData, Note) :-
    unique_subterm_member(saving_throw(_):(_ else Alt), CantripData.effects),
    format(string(Note), "on save: ~w", Alt).


%! weapon_attack(?Weapon, ?Range, ?ToHit, ?DamageRolls, ?Notes)
%
%  Stats for an attack with a given Weapon (does not check has/1).
%  The Weapon can be a base weapon as defined by the weapon/5 predicate;
%  it can also be a compound term of the form `Weapon + Enchantment`.
weapon_attack(Weapon, Range, to_hit(ToHit), FinalDamageRolls, Notes) :-
    canonicalize_item(Weapon, BaseWeapon + Enchantment ~ Variant),
    weapon(BaseWeapon, _, _, _, _),
    %weapon_base_damage_rolls(BaseWeapon, BaseDamageRolls),
    weapon_attack_ability_and_modifier(BaseWeapon, _, Mod),
    weapon_proficiency_bonus(BaseWeapon, ProfBon),
    base_weapon_range_with_bonuses(BaseWeapon, Range),
    attack_notes(BaseWeapon, Variant, Notes),
    other_bonuses_to_hit(BaseWeapon, OtherBonuses),
    ToHit is Mod + ProfBon + OtherBonuses + Enchantment,
    weapon_damage_rolls(BaseWeapon, Mod, Enchantment, FinalDamageRolls).
    %add_bonus_to_first_die(Mod + Enchantment, BaseDamageRolls, FinalDamageRolls).

base_weapon_range_with_bonuses(BaseWeapon, feet(Range)) :-
    base_weapon_range(BaseWeapon, feet(BaseRange)),
    !,
    findall(B, bonus(range(BaseWeapon) + feet(B)), RangeBonuses),
    sumlist([BaseRange|RangeBonuses], Range).
base_weapon_range_with_bonuses(BaseWeapon, Range) :-
    base_weapon_range(BaseWeapon, Range).
    % TODO for now I haven't implemented range bonuses for ranged weapons with extended range

%! weapon_variant_attack(?WeaponVariant, ?Range, ?ToHit, ?DamageRolls, ?Notes)
%
%  Stats for an attack with a given weapon variant (does not check has/1).
%  WeaponVariant must be exactly a term defined by the weapon_variant/4
%  predicate.
% TODO delete
%weapon_variant_attack(Weapon ~ Variant, Range, ToHit, DamageRolls, Notes) :-
%    weapon_variant(WeaponVariant, Weapon, ExtraDamageRolls, ExtraNotes),
%    weapon_attack(Weapon, Range, ToHit, BaseDamageRolls, BaseNotes),
%    append(BaseDamageRolls, ExtraDamageRolls, DamageRolls),
%    append(BaseNotes, ExtraNotes, Notes).

weapon_damage_rolls(BaseWeapon, Mod, Enchantment, FinalRolls) :-
    weapon_base_damage_rolls(BaseWeapon, BaseRolls),
    findall(R, bonus(extra_damage_roll(BaseWeapon,R)), AdditionalRolls),
    append(BaseRolls, AdditionalRolls, AllRolls),
    add_bonus_to_first_die(Mod + Enchantment, AllRolls, FinalRolls).

weapon_base_damage_rolls(Weapon, Rolls) :-
    bonus(override_attack_damage_rolls(Weapon, Rolls)),
    !.
weapon_base_damage_rolls(Weapon, Rolls) :-
    weapon(Weapon, _, _, Rolls, _).

%! counts_as_unarmed(?Weapon)
%
%  Unarmed strikes trivially count as unarmed.
%  But some characters have natural weapons, which may or may not count
%  as unarmed strikes. This predicate explicitly enumerates every "weapon"
%  (including basic unarmed strikes) that can be used to make unarmed strikes.
counts_as_unarmed(unarmed).
weapon(unarmed, unarmed, melee, [damage(bludgeoning,1)], []).

%! has_natural_weapon(?Weapon)
%
%  Indicates whether character intrinsically has a natural weapon such as claws, horns, etc.
has_natural_weapon(_) :- false.

%! has_unarmed(?Weapon)
%
%  True if Weapon is `unarmed`, or if it's a natural weapon that counts as unarmed.
has_unarmed(unarmed).
has_unarmed(NaturalWeapon) :-
    has_natural_weapon(NaturalWeapon),
    counts_as_unarmed(NaturalWeapon).

%! attack_variant(?Id, ?Range, ?ToHit, ?DamageFormula, ?Notes)
%
%  `Id = Name:_`, where `attack(Name,_,_,_,_)`.
attack_variant(Name:twohanded, Range, ToHit,
               [damage(Type,NewDmgTerm)|Terms], []) :-
    attack(Name, Range, ToHit, [damage(Type,Formula)|Terms], Notes),
    member(versatile(NewBaseDmg), Notes),
    select_first_subterm(_ d _, Formula, NewBaseDmg, NewDmgTerm).
meta_todo(versatile, "Also shows up when it's strictly worse than the onehanded variant.").
custom_format(Weapon:twohanded) --> format_term(Weapon), [" (2H)"].

%! attack_or_variant(?Id, ?Range, ?ToHit, ?DamageFormula, ?Notes, ?IsVariant:bool)
%
%  Disjunction between attack/5 and attack_variant/5, structured such that
%  attacks and their variants are "together" and tagged with a boolean indicating
%  whether the attack is a variant or not.
attack_or_variant(Id, Range, ToHit, DamageFormula, Notes, IsVariant) :-
    attack(BaseId, BaseRange, BaseToHit, BaseDamageFormula, BaseNotes),
    findall((BaseId:VarId)-VarRange-VarToHit-VarDamageFormula-VarNotes-true,
            attack_variant(BaseId:VarId, VarRange, VarToHit, VarDamageFormula, VarNotes),
            Variants),
    member(Id-Range-ToHit-DamageFormula-Notes-IsVariant,
           [BaseId-BaseRange-BaseToHit-BaseDamageFormula-BaseNotes-false | Variants]).

%! attack_or_variant(?Id, ?Range, ?ToHit, ?DamageFormula, ?Notes)
%
%  attack_or_variant/6 with the IsVariant parameter removed.
attack_or_variant(Id, Range, ToHit, DamageFormula, Notes) :-
    attack_or_variant(Id, Range, ToHit, DamageFormula, Notes, _).


%! add_bonus_to_first_die(+Bonus, +Rolls, -NewRolls)
add_bonus_to_first_die(Bonus, [damage(Type,Roll)|Rolls], [damage(Type,NewRoll)|Rolls]) :-
    EvaldBonus is Bonus,
    simplify_dice_sum(Roll+EvaldBonus, NewRoll).

attack_notes(Weapon, Variant, Notes) :-
    weapon(Weapon, _, _, _, BaseNotes),
    findall(Note,
            (( member(Note, BaseNotes)
             ; bonus(add_weapon_note(Weapon, Note))
             ; bonus(add_variant_weapon_note(Variant, Note))
             ; not_proficient_note(Weapon, Note)
             ),
             \+ ( bonus(remove_weapon_note(Weapon, Note))
                ; bonus(remove_variant_weapon_note(Variant, Note)))
            ),
            Notes
           ).

not_proficient_note(Weapon, "not proficient") :-
    \+ weapon_proficiency(Weapon).

%! base_weapon_range(?Weapon, ?Range)
%
%  Range is either a term `feet(N)` or `feet(N) / feet(M)` where N and
%  M are numbers. In theory you could have a different unit of length
%  instead of feet.
base_weapon_range(BaseWeapon, feet(Range)) :-
    weapon(BaseWeapon, _, melee, _, Notes),
    findall(Ft, (member(reach(feet(Ft)),Notes); bonus(reach+feet(Ft))), Fts),
    sum_list(Fts, Bonus),
    Range is 5 + Bonus.
base_weapon_range(BaseWeapon, Range) :-
    weapon(BaseWeapon, _, ranged(Range), _, _).

%! weapon_attack_ability_and_modifier(?Weapon, ?Abi, ?Mod)
%
%  Determine the ability and corresponding ability modifier used to
%  make attack and damage rolls with the given weapon.
%  Automatically picks the best one.
weapon_attack_ability_and_modifier(Weapon, Abi, Mod) :-
    findall(A, weapon_ability_candidate(Weapon, A), Abis),
    highest_ability_mod_from(Abis, Abi, Mod).

%! weapon_ability_candidate(?Weapon, ?Ability)
%
%  True iff Ability can be used to calculate to hit and damage for
%  attacks with Weapon.
weapon_ability_candidate(Weapon, str) :- weapon(Weapon, _, melee, _, _).
weapon_ability_candidate(Weapon, Abi) :- bonus(use_ability(Weapon, Abi)).
weapon_ability_candidate(Weapon, dex) :-
    weapon(Weapon, _, _, _, Notes), member(finesse, Notes).
weapon_ability_candidate(Weapon, dex) :- weapon(Weapon, _, ranged(_), _, _).

%! weapon_proficiency_bonus(?Weapon, ?ProfBon)
%
%  Calculate the proficiency bonus with Weapon (0 if not proficient).
weapon_proficiency_bonus(Weapon, ProfBon) :-
    weapon_proficiency(Weapon),
    !,
    proficiency_bonus(ProfBon).
weapon_proficiency_bonus(_, 0).

weapon_proficiency(Weapon) :-
    trait(weapon(Weapon)).
weapon_proficiency(Weapon) :-
    weapon(Weapon, Category, _, _, _),
    trait(weapon(Category)).
weapon_proficiency(Weapon) :-
    counts_as_unarmed(Weapon).
    
other_bonuses_to_hit(Weapon, TotalBonus) :-
    weapon(Weapon, _, _, _, _),
    findall(B, bonus(to_hit(Weapon) + B), Bonuses),
    sum_list(Bonuses, TotalBonus).

damage_type(acid).
damage_type(bludgeoning).
damage_type(cold).
damage_type(fire).
damage_type(force).
damage_type(lightning).
damage_type(necrotic).
damage_type(piercing).
damage_type(poison).
damage_type(psychic).
damage_type(radiant).
damage_type(slashing).
damage_type(thunder).
