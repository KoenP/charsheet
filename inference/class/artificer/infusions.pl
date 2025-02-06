:- discontiguous infusion_option/1.
infusion_option(_) :- false.

% Helper predicate.
make_variant(Variation, BaseItem, BaseItem ^ Variation).

% Infusion: Arcane propulsion armor.
infusion_option('arcane propulsion armor') :- artificer >: 14.
infusion('arcane propulsion armor') ?= "The wearer of this armor gains these benefits:

- The wearer’s walking speed increases by 5 feet.
- The armor includes gauntlets, each of which is a magic melee weapon that can be wielded only when the hand is holding nothing. The wearer is proficient with the gauntlets, and each one deals 1d8 force damage on a hit and has the thrown property, with a normal range of 20 feet and a long range of 60 feet. When thrown, the gauntlet detaches and flies at the attack’s target, then immediately returns to the wearer and reattaches.
- The armor can’t be removed against the wearer’s will.
- If the wearer is missing any limbs, the armor replaces those limbs—hands, arms, feet, legs, or similar appendages. The replacements function identically to the body parts they replace.".
bonus_source(has(_ ^ 'arcane propulsion'), speed + 5).
attack('arcane propulsion gauntlet', melee, to_hit(ToHit), [damage(force, 1 d 8)],
       [thrown(feet(20) / feet(60)), 'returns after throw']) :-
    has(_ ^ 'arcane propulsion'),
    ability_mod(str, Mod),
    proficiency_bonus(ProfBon),
    ToHit is Mod + ProfBon.
custom_format('arcane propulsion armor'(BaseArmor)) -->
    ['arcane propulsion '], format_term(BaseArmor).

inferred_has_options_source(trait(infusion('arcane propulsion armor')),
                            'equip arcane propulsion armor',
                            make_variant('arcane propulsion'),
                            base_body_armor).

% Infusion: Armor of magical strength
infusion_option('armor of magical strength').
infusion('armor of magical strength') ?= "This armor has 6 charges. The wearer can expend the armor’s charges in the following ways:

- When the wearer makes a Strength check or a Strength saving throw, it can expend 1 charge to add a bonus to the roll equal to its Intelligence modifier.
- If the creature would be knocked prone, it can use its reaction to expend 1 charge to avoid being knocked prone.

The armor regains 1d6 expended charges daily at dawn.".

res('armor of magical strength', 6) :-
    has(_ ^ 'magical strength').
restore_res('at dawn', 'armor of magical strength', restore(1 d 6)).
custom_format(BaseArmor ^ 'magical strength') -->
    format_term(BaseArmor), [' of magical strength'].

inferred_has_options_source(trait(infusion('armor of magical strength')),
                            'equip armor of magical strength',
                            make_variant('magical strength'),
                            base_body_armor).

% Infusion: Boots of the Winding Path
infusion_option('boots of the winding path') :- artificer >: 6.
infusion('boots of the winding path') ?= "While wearing these boots, a creature can teleport up to 15 feet as a bonus action to an unoccupied space the creature can see. The creature must have occupied that space at some point during the current turn.".

% Infusion: Enhanced Arcane Focus
infusion_option('enhanced arcane focus').
infusion('enhanced arcane focus') ?= "While holding this item, a creature gains a +1 bonus to spell attack rolls. In addition, the creature ignores half cover when making a spell attack.

The bonus increases to +2 when you reach 10th level in this class.".
magic_item('enhanced arcane focus').
bonus_source(has('enhanced arcane focus'), 'spell attack rolls' + N) :-
    (artificer >: 10 -> N = 2 ; N = 1).
% TODO: make this bonus parenthethised somehow.
bonus_source(has('enhanced arcane focus'), modify_spell(_, Spell, Goal)) :-
    known_spell(_, Spell),
    spell_property(Spell, effects, Effects),
    subterm_member(spell_attack_roll(_), Effects),
    Goal = add_spell_effects(["ignore half cover"]).

inferred_has_options_source(trait(infusion('enhanced arcane focus')),
                            'equip enhanced arcane focus',
                            const('enhanced arcane focus'),
                            toggle).

% Infusion: Enhanced Defense
infusion_option('enhanced defense').
infusion('enhanced defense') ?= "Item: A suit of armor or a shield

A creature gains a +1 bonus to Armor Class while wearing (armor) or wielding (shield) the infused item.

The bonus increases to +2 when you reach 10th level in this class.".

custom_format((Armor + N) ^ 'enhanced defense') -->
    ["enhanced defense "], format_term(Armor), [" "], format_bonus(N).
% TODO verify if the armor is a valid body armor or shield?
inferred_has_options_source(trait(infusion('enhanced defense')),
                            'equip enhanced defense armor',
                            make_enhanced_defense_armor(N),
                            shield_or_base_body_armor) :-
    class_level(artificer:L),
    (L < 10 -> N = 1 ; N = 2).
make_enhanced_defense_armor(N, Armor, (Armor + N) ^ 'enhanced defense').

% Infusion: Enhanced weapon
infusion_option('enhanced weapon').
infusion('enhanced weapon') ?= "This magic weapon grants a +1 bonus to attack and damage rolls made with it.
The bonus increases to +2 when you reach 10th level in this class.".
custom_format((_ + N) ^ 'enhanced weapon') -->
    ["enhanced "], format_term(BaseWeapon).
inferred_has_options_source(trait(infusion('enhanced weapon')),
                            'equip enhanced weapon',
                            make_enhanced_weapon(N),
                            simple_or_martial_weapon_option) :-
    class_level(artificer:L),
    (L < 10 -> N = 1 ; N = 2).
make_enhanced_weapon(N, BaseWeapon, (BaseWeapon + N) ^ 'enhanced weapon').

simple_or_martial_weapon_option(Weapon) :-
    weapon(Weapon, Category, _, _, _),
    (Category = simple ; Category = martial).
