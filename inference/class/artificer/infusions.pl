:- discontiguous infusion_option/1.
infusion_option(_) :- false.

% Helper predicate.
make_variant(Variation, BaseItem, BaseItem ~ Variation).
make_variant(Variation, Bonus, BaseItem, BaseItem + Bonus ~ Variation).

% ------------------------------------------------------------------------------
% Infusion: Arcane propulsion armor.
infusion_option('arcane propulsion armor') :- artificer >: 14.
infusion('arcane propulsion armor') ?= "The wearer of this armor gains these benefits:

- The wearer’s walking speed increases by 5 feet.
- The armor includes gauntlets, each of which is a magic melee weapon that can be wielded only when the hand is holding nothing. The wearer is proficient with the gauntlets, and each one deals 1d8 force damage on a hit and has the thrown property, with a normal range of 20 feet and a long range of 60 feet. When thrown, the gauntlet detaches and flies at the attack’s target, then immediately returns to the wearer and reattaches.
- The armor can’t be removed against the wearer’s will.
- If the wearer is missing any limbs, the armor replaces those limbs—hands, arms, feet, legs, or similar appendages. The replacements function identically to the body parts they replace.".
bonus_source(has(_ ~ 'arcane propulsion'), speed + 5).
attack('arcane propulsion gauntlet', melee, to_hit(ToHit), [damage(force, 1 d 8)],
       [thrown(feet(20) / feet(60)), 'returns after throw']) :-
    has(_ ~ 'arcane propulsion'),
    ability_mod(str, Mod),
    proficiency_bonus(ProfBon),
    ToHit is Mod + ProfBon.
custom_format('arcane propulsion armor'(BaseArmor)) -->
    ['arcane propulsion '], format_term(BaseArmor).

inferred_has_options_source(trait(infusion('arcane propulsion armor')),
                            'equip arcane propulsion armor',
                            make_variant('arcane propulsion'),
                            base_body_armor).

% ------------------------------------------------------------------------------
% Infusion: Armor of magical strength
infusion_option('armor of magical strength').
infusion('armor of magical strength') ?= "This armor has 6 charges. The wearer can expend the armor’s charges in the following ways:

- When the wearer makes a Strength check or a Strength saving throw, it can expend 1 charge to add a bonus to the roll equal to its Intelligence modifier.
- If the creature would be knocked prone, it can use its reaction to expend 1 charge to avoid being knocked prone.

The armor regains 1d6 expended charges daily at dawn.".

res('armor of magical strength charges', 6) :-
    has(_ ~ 'magical strength').
restore_res('at dawn', 'armor of magical strength charges', restore(1 d 6)).
custom_format(BaseArmor ~ 'magical strength') -->
    format_term(BaseArmor), [' of magical strength'].

inferred_has_options_source(trait(infusion('armor of magical strength')),
                            'equip armor of magical strength',
                            make_variant('magical strength'),
                            base_body_armor).

% ------------------------------------------------------------------------------
% Infusion: Boots of the Winding Path
infusion_option('boots of the winding path') :- artificer >: 6.
infusion('boots of the winding path') ?= "While wearing these boots, a creature can teleport up to 15 feet as a bonus action to an unoccupied space the creature can see. The creature must have occupied that space at some point during the current turn.".

% ------------------------------------------------------------------------------
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

% ------------------------------------------------------------------------------
% Infusion: Enhanced Defense
infusion_option('enhanced defense').
infusion('enhanced defense') ?= "Item: A suit of armor or a shield

A creature gains a +1 bonus to Armor Class while wearing (armor) or wielding (shield) the infused item.

The bonus increases to +2 when you reach 10th level in this class.".

custom_format(Armor + N ~ 'enhanced defense') -->
    ["enhanced defense "], format_term(Armor), [" "], format_bonus(N).
% TODO verify if the armor is a valid body armor or shield?
inferred_has_options_source(trait(infusion('enhanced defense')),
                            'equip enhanced defense armor',
                            make_enhanced_defense_armor(N),
                            shield_or_base_body_armor) :-
    class_level(artificer:L),
    (L < 10 -> N = 1 ; N = 2).
make_enhanced_defense_armor(N, Armor, Armor + N ~ 'enhanced defense').

% ------------------------------------------------------------------------------
% Infusion: Enhanced weapon
infusion_option('enhanced weapon').
infusion('enhanced weapon') ?= "This magic weapon grants a +1 bonus to attack and damage rolls made with it.
The bonus increases to +2 when you reach 10th level in this class.".
custom_format(BaseWeapon ~ 'enhanced weapon') -->
    ["enhanced "], format_term(BaseWeapon).
inferred_has_options_source(trait(infusion('enhanced weapon')),
                            'equip enhanced weapon',
                            make_enhanced_weapon(N),
                            weapon_of_category(simple) \/ weapon_of_category(martial)) :-
    class_level(artificer:L),
    (L < 10 -> N = 1 ; N = 2).
make_enhanced_weapon(N, BaseWeapon, BaseWeapon + N ~ 'enhanced weapon').

% ------------------------------------------------------------------------------
% Infusion: Helm of Awareness
infusion_option('helm of awareness') :- artificer >: 10.
trait_source(trait(infusion('homunculus servant')), 'homunculus servant').
infusion('helm of awareness') ?= "While wearing this helmet, a creature has advantage on initiative rolls. In addition, the wearer can’t be surprised, provided it isn’t incapacitated.".

% ------------------------------------------------------------------------------
% Infusion: Homunculus Servant
infusion_option('homunculus servant').
infusion('homunculus servant') ?= "You learn intricate methods for magically creating a special homunculus that serves you. The item you infuse serves as the creature’s heart, around which the creature’s body instantly forms.

You determine the homunculus’s appearance. Some artificers prefer mechanical-looking birds, whereas some like winged vials or miniature, animate cauldrons.

The homunculus is friendly to you and your companions, and it obeys your commands. See this creature’s game statistics in the Homunculus Servant stat block, which uses your proficiency bonus (PB) in several places.

In combat, the homunculus shares your initiative count, but it takes its turn immediately after yours. It can move and use its reaction on its own, but the only action it takes on its turn is the Dodge action, unless you take a bonus action on your turn to command it to take another action. That action can be one in its stat block or some other action. If you are incapacitated, the homunculus can take any action of its choice, not just Dodge.

The homunculus regains 2d6 hit points if the mending spell is cast on it. If you or the homunculus dies, it vanishes, leaving its heart in its space.".

('homunculus servant' ?= Desc) :-
    ability_mod(int, IntMod),
    class_level(artificer:Lvl),
    proficiency_bonus(ProfBon),
    spell_attack_modifier(artificer, ToHitVal),
    fmt(format_bonus(ToHitVal), ToHit),
    HP is 1 + IntMod + Lvl,
    DexST is 2 + ProfBon,
    PP is 10 + ProfBon*2,
    format(string(Desc),
           "**Tiny construct**

| HP | HD   | AC | Walk spd. | Fly spd. |
|----|------|----|-----------|----------|
| ~w | ~wd4 | 13 | 20 ft.    | 30 ft.   |

| STR    | DEX     | CON     | INT     | WIS     | CHA    |
|--------|---------|---------|---------|---------|--------|
| 4 (-3) | 15 (+2) | 12 (+1) | 10 (+0) | 10 (+0) | 7 (-2) |

**Saving throws** Dex +~w

**Skills** Perception +~w, Stealth +~w

**Immunities** poison damage, poisoned condition, exhausted condition

**Senses** darkvision 60 ft., passive Perception +~w

**Languages** understands the languages you speak

**Evasion.** If subjected to an effect that lets it make a DEX ST to take half damage, takes no damage instead (when not incapacitated).

**Action: Force Strike.** ~w to hit, 30 ft. range, 1d4 + ~w force damage

**Reaction: Channel Magic.** Deliver a spell with range of touch. Homunculus must be within 120 ft. of you.
",
           [HP, Lvl, DexST, ProfBon, ProfBon, PP, ToHit, ProfBon]).

% ------------------------------------------------------------------------------
% Infusion: Mind Sharpener
infusion_option('mind sharpener').
infusion('mind sharpener') ?= "The infused item can send a jolt to the wearer to refocus their mind. The item has 4 charges. When the wearer fails a Constitution saving throw to maintain concentration on a spell, the wearer can use its reaction to expend 1 of the item’s charges to succeed instead. The item regains 1d4 expended charges daily at dawn.".

inferred_has_options_source(trait(infusion('mind sharpener')),
                            'equip mind sharpener',
                            make_variant('mind sharpener'),
                            id(robe) \/ base_body_armor).

custom_format(Armor ~ 'mind sharpener') -->
    ["mind sharpener "], format_term(Armor).

res('mind sharpener charges', 4) :- has(_ ~ 'mind sharpener').
restore_res('at dawn', 'mind sharpener charges', restore(1 d 4)).

% ------------------------------------------------------------------------------
% Infusion: Radiant Weapon
infusion_option('radiant weapon') :- artificer >: 6.
infusion('radiant weapon') ?= "This magic weapon grants a +1 bonus to attack and damage rolls made with it. While holding it, the wielder can take a bonus action to cause it to shed bright light in a 30-foot radius and dim light for an additional 30 feet. The wielder can extinguish the light as a bonus action.

The weapon has 4 charges. As a reaction immediately after being hit by an attack, the wielder can expend 1 charge and cause the attacker to be blinded until the end of the attacker’s next turn, unless the attacker succeeds on a Constitution saving throw against your spell save DC. The weapon regains 1d4 expended charges daily at dawn.".

inferred_has_options_source(trait(infusion('radiant weapon')),
                            'equip radiant weapon',
                            make_variant(radiant, 1),
                            weapon_of_category(simple) \/ weapon_of_category(martial)).

custom_format(BaseWeapon ~ radiant) -->
    ["radiant "], format_term(BaseWeapon).

res('radiant weapon charges', 4) :-
    has(_ ~ radiant).
restore_res('at dawn', 'radiant weapon charges', 1 d 4).

% ------------------------------------------------------------------------------
% Infusion: Repeating Shot
infusion_option('repeating shot').
infusion('repeating shot') ?= "This magic weapon grants a +1 bonus to attack and damage rolls made with it when it’s used to make a ranged attack, and it ignores the loading property if it has it.

If the weapon lacks ammunition, it produces its own, automatically creating one piece of magic ammunition when the wielder makes a ranged attack with it. The ammunition created by the weapon vanishes the instant after it hits or misses a target.".

inferred_has_options_source(trait(infusion('repeating shot')),
                            'equip repeating shot weapon',
                            make_variant('repeating shot', 1),
                            repeating_shot_weapon).
repeating_shot_weapon(Weapon) :-
    weapon_with_ammunition(Weapon),
    weapon_of_category(Category, Weapon),
    (Category = simple ; Category = martial).

bonus_source(has(_ ~ 'repeating shot'),
             remove_variant_weapon_note('repeating shot', loading)).

custom_format(BaseWeapon ~ 'repeating shot') -->
    ["repeating shot "], format_term(BaseWeapon).

% ------------------------------------------------------------------------------
% Infusion: Replicate Magic Item
infusion_option('replicate magic item').
infusion('replicate magic item') ?= "Using this infusion, you replicate a particular magic item. You can learn this infusion multiple times; each time you do so, choose a magic item that you can make with it, picking from the Replicable Items tables. A table’s title tells you the level you must be in the class to choose an item from the table. Alternatively, you can choose the magic item from among the common magic items in the game, not including potions or scrolls.

In the tables, an item’s entry tells you whether the item requires attunement. See the item’s description in the Dungeon Master’s Guide for more information about it, including the type of object required for its making.".

% ------------------------------------------------------------------------------
% Infusion: Repulsion Shield
infusion_option('repulsion shield') :- artificer >: 6.
infusion('repulsion shield') ?= "A creature gains a +1 bonus to Armor Class while wielding this shield.

The shield has 4 charges. While holding it, the wielder can use a reaction immediately after being hit by a melee attack to expend 1 of the shield’s charges and push the attacker up to 15 feet away. The shield regains 1d4 expended charges daily at dawn.".

inferred_has_options_source(trait(infusion('repulsion shield')),
                            'equip repulsion shield',
                            const(shield+1 ~ repulsion),
                            toggle).

custom_format(shield+1 ~ repulsion) --> ["repulsion shield + 1"].

res('repulsion shield charges', 4) :- has(shield+1 ~ repulsion).
restore_res('at dawn', 'repulsion shield charges', 1 d 4).

% ------------------------------------------------------------------------------
% Infusion: Resistant Armor
infusion_option('resistant armor') :- artificer >: 6.
infusion('resistant armor') ?= "While wearing this armor, a creature has resistance to one of the following damage types, which you choose when you infuse the item: acid, cold, fire, force, lightning, necrotic, poison, psychic, radiant, or thunder.".

options_source(
    trait(infusion('resistant armor')),
    'resistant armor: resist damage type',
    from_list([acid, cold, fire, force, lightning, necrotic, poison, psychic, radiant, thunder])).

inferred_has_options_source(trait(infusion('resistant armor')),
                            'equip resistant armor',
                            make_variant(resistance(Type)),
                            base_body_armor) :-
    choice(trait(infusion('resistant armor')), 'resistant armor: resist damage type', Type).

trait_source(has(_ ~ resistance(Type)), resistance(Type, half)).

custom_format(BaseArmor ~ resistance(Type)) -->
    format_term(BaseArmor), [" of "], [Type], [" resistance"].

% ------------------------------------------------------------------------------
% Infusion: Returning Weapon
infusion_option('returning weapon').
infusion('returning weapon') ?= "This magic weapon grants a +1 bonus to attack and damage rolls made with it, and it returns to the wielder’s hand immediately after it is used to make a ranged attack.".

inferred_has_options_source(trait(infusion('returning weapon')),
                            'equip returning weapon',
                            make_variant(returning, 1),
                            returning_weapon_candidate).
returning_weapon_candidate(Weapon) :-
    weapon_of_category(Category, Weapon),
    (Category = simple ; Category = martial),
    base_weapon_note(Weapon, thrown(_)).

bonus_source(has(_ ~ returning),
             add_variant_weapon_note(returning, "returns to hand after ranged attack")).

custom_format(Weapon ~ returning) -->
    format_term(Weapon), [" of returning"].

% ------------------------------------------------------------------------------
% Infusion: Spell-Refueling Ring
infusion_option('spell-refueling ring') :- artificer >: 6.
infusion('spell-refueling ring') ?= "While wearing this ring, the creature can recover one expended spell slot as an action. The recovered slot can be of 3rd level or lower. Once used, the ring can’t be used again until the next dawn.".

inferred_has_options_source(trait(infusion('spell-refueling ring')),
                            'equip spell-refueling ring',
                            const('spell-refueling ring'),
                            toggle).

res('spell-refueling ring charge', 1) :- has('spell-refueling ring').
restore_res('at dawn', 'spell-refueling ring charge', 'full restore').
