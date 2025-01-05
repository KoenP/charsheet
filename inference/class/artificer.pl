class_option(artificer).
hd_per_level(artificer, 1 d 8).
initial_class_base_hp(artificer, 8).
max_hp_per_level(artificer, 1 d 8).
choose_subclass_level(artificer:3).
caster(artificer, 1/2).
spellcasting_ability(artificer, int).
asi_level(artificer:L) :- default_asi_level(L).
class_saving_throw(artificer, con).
class_saving_throw(artificer, int).


class_skill_list(artificer, [arcana, history, investigation, medicine, nature,
                             perception, 'sleight of hand']).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Level 1 features. I don't think there are any initial-class-exclusive
% features for artificers.
traits_from_source(artificer >: 1,
                   [armor(light), armor(medium), armor(shield),
                    weapon(simple), tool('thieves\' tools'), tool('tinker\'s tools')]).
trait_options_source(artificer >: 1, 'tool proficiency', wrap(tool), artisans_tools).

trait_source(artificer >: 1, 'magical tinkering').

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Features gained from leveling up.
trait_source(artificer >: 3, 'the right tool for the job').

trait_source(artificer >: 6, expertise(tool(Tool))) :-
    trait(tool(Tool)). % TODO does this terminate?

trait_source(artificer >: 7, 'flash of genius').
res('flash of genius', N) :-
    trait('flash of genius'),
    ability_mod(int, IntMod),
    N is max(1, IntMod).
restore_res('long rest', 'flash of genius', 'full restore').

trait_source(artificer >: 10, 'magic item adept').

trait_source(artificer >: 11, 'spell-storing item'(Uses)) :-
    ability_mod(int, IntMod),
    Uses is max(2, IntMod).

trait_source(artificer >: 14, 'magic item savant').

trait_source(artificer >: 18, 'magic item master').

trait_source(artificer >: 20, 'soul of artifice').


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Spellcasting.
trait_source(artificer >: 1, spellcasting_focus(tools)).
trait_source(artificer >: 2, spellcasting_focus('infused item')).

% Artificers get to pick cantrips.
known_spell(artificer, int, always, [], no, Spell) :-
    class_choice(artificer, cantrip, Spell).
options_source(artificer >: 1, cantrip, 2 unique_from class_cantrip(artificer)).
options_source(artificer >: L, cantrip, class_cantrip(artificer)) :- L=4 ; L=10.

% Artificers know all proper spells on their spell list.
% These always need to be prepared, with the exception of specialization spells.
known_spell(artificer, int, 'when prepared', [slot], Ritual, Spell) :-
    learnable_proper_spell(artificer, Spell),
    \+ (subclass(artificer(Specialization)), artificer_specialization_spell(Specialization, Spell)),
    spell_property(Spell, ritual, Ritual).

% Specialization spells are always prepared.
known_spell(artificer, int, always, [slot], Ritual, Spell) :-
    subclass(artificer(Specialization)),
    artificer_specialization_spell(Specialization, Spell),
    learnable_proper_spell(artificer, Spell),
    spell_property(Spell, ritual, Ritual).

% Add specialization spells to the artificer spell list.
extend_class_spell_list(artificer, Spell) :-
    subclass(artificer(Specialization)),
    artificer_specialization_spell(Specialization, Spell).

:- discontiguous artificer_specialization_spell/2.
artificer_specialization_spell(_,_) :- false.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Infusions.
trait_source(artificer >: 2, infuse_items(N)) :-
    class_level(artificer:L),
    ordered_lookup_largest_leq([2 -> 2, 6 -> 3, 10 -> 4, 14 -> 5, 18 -> 6],
                               L, N).

replaceable_class_options(artificer:2, 'infusion',
                          4 unique_from infusion_option).
replaceable_class_options(artificer:L, 'infusion',
                          2 unique_from infusion_option) :-
    member(L, [6, 10, 14, 18]).
replace_at_class_level(artificer:L, 'infusion', 1,
                       infusion_option) :-
    between(3, 20, L).
custom_format(infusion(Inf)) --> ['infusion: '], [Inf].

trait_source(artificer >: L, infusion(Inf)) :-
    find_choice_level(artificer:L, 'infusion', Inf).

:- discontiguous infusion_option/1.
infusion_option(_) :- false.

% TODO: desctiptions, specializations, infusions

% Infusion: Arcane propulsion armor.
infusion_option('arcane propulsion armor') :- artificer >: 14.
infusion('arcane propulsion armor') ?= "The wearer of this armor gains these benefits:

- The wearer’s walking speed increases by 5 feet.
- The armor includes gauntlets, each of which is a magic melee weapon that can be wielded only when the hand is holding nothing. The wearer is proficient with the gauntlets, and each one deals 1d8 force damage on a hit and has the thrown property, with a normal range of 20 feet and a long range of 60 feet. When thrown, the gauntlet detaches and flies at the attack’s target, then immediately returns to the wearer and reattaches.
- The armor can’t be removed against the wearer’s will.
- If the wearer is missing any limbs, the armor replaces those limbs—hands, arms, feet, legs, or similar appendages. The replacements function identically to the body parts they replace.".
body_armor_variant('arcane propulsion armor'(BaseArmor), BaseArmor).
bonus_source(has('arcane propulsion armor'(_)), speed + 5).
attack('arcane propulsion gauntlet', melee, to_hit(ToHit), [damage(force, 1 d 8)],
       [thrown(feet(20) / feet(60)), 'returns after throw']) :-
    has('arcane propulsion armor'(_)),
    ability_mod(str, Mod),
    proficiency_bonus(ProfBon),
    ToHit is Mod + ProfBon.
custom_format('arcane propulsion armor'(BaseArmor)) -->
    ['arcane propulsion '], format_term(BaseArmor).

% Infusion: Armor of magical strength
infusion_option('armor of magical strength').
infusion('armor of magical strength') ?= "This armor has 6 charges. The wearer can expend the armor’s charges in the following ways:

- When the wearer makes a Strength check or a Strength saving throw, it can expend 1 charge to add a bonus to the roll equal to its Intelligence modifier.
- If the creature would be knocked prone, it can use its reaction to expend 1 charge to avoid being knocked prone.

The armor regains 1d6 expended charges daily at dawn.".
body_armor_variant('armor of magical strength'(BaseArmor), BaseArmor).
res('armor of magical strength', 6) :-
    has('armor of magical strength'(_)).
restore_res('at dawn', 'armor of magical strength', restore(1 d 6)).
custom_format('armor of magical strength'(BaseArmor)) -->
    format_term(BaseArmor), [' of magical strength'].

% TODO more infusions.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Artificer specialist: alchemist
subclass_option(artificer, alchemist).

% If character already has alchemist's supplies proficiency from another source,
% pick a different set of artisan's tools.
trait_options_source(artificer(alchemist) >: 3, 'artisan\'s tools', wrap(tool),
                     artisans_tools) :- 
    trait(Source, tool('alchemist\'s supplies')),
    Source \= artificer(alchemist) >: 3.
% If the character does not already have alchemist's supplies proficiency,
% add that.
trait_source(artificer(alchemist) >: 3, tool('alchemist\'s supplies')) :-
    \+ (trait(Source, tool('alchemist\'s supplies')),
        Source \= artificer(alchemist) >: 3).

% Alchemist specialization spells.
artificer_specialization_spell(alchemist, 'healing word').
artificer_specialization_spell(alchemist, 'ray of sickness').
artificer_specialization_spell(alchemist, 'flaming sphere').
artificer_specialization_spell(alchemist, 'melf\'s acid arrow').
artificer_specialization_spell(alchemist, 'gaseous form').
artificer_specialization_spell(alchemist, 'mass healing word').
artificer_specialization_spell(alchemist, blight).
artificer_specialization_spell(alchemist, 'death ward').
artificer_specialization_spell(alchemist, cloudkill).
artificer_specialization_spell(alchemist, 'raise dead').

% Other alchemist features.
trait_source(artificer(alchemist) >: 3, 'experimental elixir').

trait_source(artificer(alchemist) >: 5, 'alchemical savant').
alchemical_savant_element(acid).
alchemical_savant_element(fire).
alchemical_savant_element(necrotic).
alchemical_savant_element(poison).
bonus_source(trait('alchemical savant'),
             modify_spell(artificer, DamageSpell, Goal)) :-
    (known_spell(artificer, DamageSpell) ; known_spell(artificer(_), DamageSpell)),
    spell_property(DamageSpell, effects, Effects),
    ( subterm_member(damage(Element, _), Effects),
      alchemical_savant_element(Element),
      Effect = damage
    ; subterm_member(heal(_), Effects),
      Effect = heal
    ),
    ability_mod(int, IntMod),
    Bonus is max(1, IntMod),
    Goal = modify_spell_field(effects, apply_alchemical_savant(Effect, Bonus)).
apply_alchemical_savant(damage, Bonus, OldEffects, NewEffects) :-
    (  contains_multiple_damage_rolls(OldEffects)
    -> atomics_to_string(["add +", Bonus, " to one damage roll"], New),
       append(OldEffects, [New], NewEffects)
    ;  select_subterm(damage(Element,Dice), OldEffects, damage(Element,NewDice), NewEffects),
       simplify_dice_sum(Dice + Bonus, NewDice)
    ).
apply_alchemical_savant(heal, Bonus, OldEffects, NewEffects) :-
    (  contains_multiple_healing_rolls(OldEffects)
    -> atomics_to_string(["add +", Bonus, " to one healing roll"], New),
       append(OldEffects, [New], NewEffects)
    ;  select_subterm(heal(Dice), OldEffects, heal(NewDice), NewEffects),
       simplify_dice_sum(Dice + Bonus, NewDice)
    ).
custom_format(modify_spell_field(effects, apply_alchemical_savant(Effect, Bonus))) -->
    ['+'], [Bonus], [' to one '], [Effect], [' roll'].
    
trait_source(artificer(alchemist) >: 9, 'restorative reagents').
res('restorative reagents: lesser restoration', N) :-
    trait('restorative reagents'),
    ability_mod(int, IntMod),
    N is max(1, IntMod).
restore_res('long rest', 'alchemist lesser restoration', 'full restore').
    
trait_source(artificer(alchemist) >: 15, 'chemical mastery').
traits_from_source(trait('chemical mastery'), [resistance(acid, half),
                                               resistance(poison, half),
                                               condition_immunity(poisoned)
                                              ]).
res('chemical mastery: heal', 1) :- trait('chemical mastery').
restore_res('long rest', 'chemical mastery: heal', 'full restore').
res('chemical mastery: greater restoration', 1) :- trait('chemical mastery').
restore_res('long rest', 'chemical mastery: greater restoration', 'full restore').

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Artificer specialist: artillerist
subclass_option(artificer, artillerist).

% If character already has woodcarver's tools proficiency from another source,
% pick a different set of artisan's tools.
trait_options_source(artificer(artillerist) >: 3, 'artisan\'s tools', wrap(tool),
                     artisans_tools) :- 
    trait(Source, tool('woodcarver\'s tools')),
    Source \= artificer(artillerist) >: 3.
% If the character does not already have woodcarver's tools proficiency,
% add that.
trait_source(artificer(artillerist) >: 3, tool('woodcarver\'s tools')) :-
    \+ (trait(Source, tool('woodcarver\'s tools')),
        Source \= artificer(artillerist) >: 3).

% Artillerist spells.
artificer_specialization_spell(artillerist, shield).
artificer_specialization_spell(artillerist, thunderwave).
artificer_specialization_spell(artillerist, 'scorching ray').
artificer_specialization_spell(artillerist, shatter).
artificer_specialization_spell(artillerist, fireball).
artificer_specialization_spell(artillerist, 'wind wall').
artificer_specialization_spell(artillerist, 'ice storm').
artificer_specialization_spell(artillerist, 'wall of fire').
artificer_specialization_spell(artillerist, 'cone of cold').
artificer_specialization_spell(artillerist, 'wall of force').

% Eldritch cannon.
trait_source(artificer(artillerist) >: 3, 'eldritch cannon').

eldritch_cannon_type(flamethrower).
eldritch_cannon_type('force ballista').
eldritch_cannon_type(protector).
trait_source(trait('eldritch cannon'), 'eldritch cannon'(Type)) :-
    eldritch_cannon_type(Type).
custom_format('eldritch cannon'(Type)) --> ["eldritch cannon: "], [Type].

trait_source(artificer(artillerist) >: 9, 'explosive cannon').

res('free eldritch cannon', 1) :- trait('eldritch cannon').
restore_res('long rest', 'free eldritch cannon', 'full restore').
res('eldritch cannon hp', HP) :-
    trait('eldritch cannon'),
    eldritch_cannon_hp(HP).
eldritch_cannon_hp(HP) :-
    class_level(artificer:L),
    HP is L*5.
eldritch_cannon_damage_dice(2) :- \+ trait('explosive cannon').
eldritch_cannon_damage_dice(3) :- trait('explosive cannon').
eldritch_cannon_action(flamethrower,
                       'Activate',
                       "bonus action within 60 ft of cannon",
                       in(15 ft cone):[saving_throw(dex):(damage(fire, N d 8) else half),
                                       "ignite flammable objects"
                                      ]) :-
    eldritch_cannon_damage_dice(N).
eldritch_cannon_action('force ballista',
                       'Activate',
                       "bonus action within 60 ft of cannon",
                       in(feet(120)):custom_attack_roll(ToHit):[damage(force, N d 8), push(feet(5))]) :-
    eldritch_cannon_damage_dice(N),
    spell_attack_modifier(artificer, ToHit).
eldritch_cannon_action(protector,
                       'Activate',
                       "bonus action within 60 ft of cannon",
                       in(10 ft sphere):'temp hp' + (1 d 8 + Bonus)) :-
    ability_mod(int, Mod),
    Bonus is max(1, Mod).

%eldritch_cannon_action(_, detonate, ) :-
%    trait('explosive cannon').
    
% Other artillerist features.
trait_source(artificer(artillerist) >: 5, 'arcane firearm').
bonus_source(trait('arcane firearm'),
             modify_spell(artificer, Spell, Goal)) :-
    known_spell(artificer, Spell),
    spell_property(Spell, effects, Effects),
    subterm_member(damage(_,_), Effects), 
    Goal = modify_spell_field(effects, apply_arcane_firearm).
apply_arcane_firearm(OldEffects, NewEffects) :-
    \+ contains_multiple_damage_rolls(OldEffects),
    select_subterm(damage(Element, Dice), OldEffects,
                   damage(Element, NewDice), NewEffects),
    simplify_dice_sum(Dice + 1 d 8, NewDice).
apply_arcane_firearm(OldEffects, NewEffects) :-
    contains_multiple_damage_rolls(OldEffects),
    atomics_to_string(["add +1d8 to one damage roll"], New),
    append(OldEffects, [New], NewEffects).
% TODO: at the moment it's not clear when this has been applied to the dice roll displayed on the card vs when it has not been applied yet.
custom_format(modify_spell_field(effects, apply_arcane_firearm)) -->
    ["Add +1d8 to one damage roll."].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

infused_items(_) ?= "Whenever you finish a long rest, you can touch a non-magical object and imbue it with one of your infusions, turning it into a magic item. An infusion works on only certain kinds of objects, as specified in the infusion’s description. If the item requires attunement, you can attune yourself to it the instant you infuse the item. If you decide to attune to the item later, you must do so using the normal process for attunement (see “Attunement” in chapter 7 of the Dungeon Master’s Guide).

Your infusion remains in an item indefinitely, but when you die, the infusion vanishes after a number of days have passed equal to your Intelligence modifier (minimum of 1 day). The infusion also vanishes if you give up your knowledge of the infusion for another one.

You can infuse more than one nonmagical object at the end of a long rest; the maximum number of objects appears in the Infused Items column of the Artificer table. You must touch each of the objects, and each of your infusions can be in only one object at a time. Moreover, no object can bear more than one of your infusions at a time. If you try to exceed your maximum number of infusions, the oldest infusion immediately ends, and then the new infusion applies.

If an infusion ends on an item that contains other things, like a bag of holding, its contents harmlessly appear in and around its space.".

'eldritch cannon' ?= "You've learned how to create a magical cannon. Using woodcarver’s tools or smith’s tools, you can take an action to magically create a Small or Tiny eldritch cannon in an unoccupied space on a horizontal surface within 5 feet of you. A Small eldritch cannon occupies its space, and a Tiny one can be held in one hand.

Once you create a cannon, you can’t do so again until you finish a long rest or until you expend a spell slot to create one. You can have only one cannon at a time and can’t create one while your cannon is present.

The cannon is a magical object. Regardless of size, the cannon has an AC of 18 and a number of hit points equal to five times your artificer level. It is immune to poison damage and psychic damage. If it is forced to make an ability check or a saving throw, treat all its ability scores as 10 (+0). If the mending spell is cast on it, it regains 2d6 hit points. It disappears if it is reduced to 0 hit points or after 1 hour. You can dismiss it early as an action.

When you create the cannon, you determine its appearance and whether it has legs. You also decide which type it is, choosing from the options on the Eldritch Cannons table. On each of your turns, you can take a bonus action to cause the cannon to activate if you are within 60 feet of it. As part of the same bonus action, you can direct the cannon to walk or climb up to 15 feet to an unoccupied space, provided it has legs.".

eldritch_cannon_stat_block(Str) :-
    eldritch_cannon_hp(HP),
    format(
        string(Str),
"**Small/tiny construct**

| HP | AC | Abi,ST | Spd (walk,climb) |
|----|----|--------|------------------|
| ~w | 18 | +0     | 15 ft. (if legs) |

**Damage Immunities**: poison, psychic

",
        [HP]).

format_eldritch_cannon_action(Type, Action, Condition) -->
    {eldritch_cannon_action(Type, Action, Condition, Desc)},
    ["\n\n**"], [Action], ["**"],
    ["("], [Condition], ["): "],
    format_effect(Desc).

('eldritch cannon'(Type) ?= Str) :-
    eldritch_cannon_stat_block(StatBlock),
    eldritch_cannon_type(Type), % ground Type
    findall(Desc, fmt(format_eldritch_cannon_action(Type, _, _), Desc), Descs),
    atomics_to_string([StatBlock|Descs], Str).
