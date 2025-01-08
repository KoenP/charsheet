:- [artificer/infusions].

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
trait_source(artificer(artillerist) >: 9, 'explosive cannon').
trait_source(artificer(artillerist) >: 15, 'fortified position').

eldritch_cannon_type(flamethrower).
eldritch_cannon_type('force ballista').
eldritch_cannon_type(protector).
trait_source(trait('eldritch cannon'), 'eldritch cannon'(Type)) :-
    eldritch_cannon_type(Type).
custom_format('eldritch cannon'(Type)) --> ["eldritch cannon: "], [Type].

res('free eldritch cannon', 1) :- trait('eldritch cannon').
restore_res('long rest', 'free eldritch cannon', 'full restore').
res('eldritch cannon hp', HP) :-
    trait('eldritch cannon'),
    eldritch_cannon_hp(HP).
res('second eldritch cannon hp', HP) :-
    trait('fortified position'),
    eldritch_cannon_hp(HP).

eldritch_cannon_hp(HP) :-
    class_level(artificer:L),
    HP is L*5.
eldritch_cannon_damage_dice(2) :- \+ trait('explosive cannon').
eldritch_cannon_damage_dice(3) :- trait('explosive cannon').

eldritch_cannon_note(flamethrower, Desc) :-
    spell_save_dc(artificer, DC),
    eldritch_cannon_damage_dice(N),
    format(string(Desc),
           "**Activate.** Use a bonus action when you are within 60 ft. of cannon. In 15 ft. cone centered on the cannon, all creatures make DC ~w DEX saving throw; on failure, take ~wd8 fire damage, else take half. Ignites flammable objects in the area of effect.\n\n",
           [DC, N]).

eldritch_cannon_note('force ballista', Desc) :-
    spell_attack_modifier(artificer, AttackMod),
    fmt(format_bonus(AttackMod), AttackModStr),
    eldritch_cannon_damage_dice(N),
    format(string(Desc),
           "**Activate.** Use a bonus action when you are within 60 ft. of cannon. Make a spell attack roll (~w to hit) against a target within 120 ft. of cannon. On hit, deal ~wd8 force damage and push target 5 ft. back.\n\n",
           [AttackModStr, N]).
eldritch_cannon_note(protector, Desc) :-
    ability_mod(int, Mod),
    Bonus is max(1, Mod),
    format(string(Desc),
           "**Activate.** Use a bonus action when you are within 60 ft. of cannon. In 10 ft. sphere centered on cannon, all allies (including cannon) gain 1d8 + ~w temporary hit points.\n\n",
           [Bonus]).

eldritch_cannon_note(_, Desc) :-
    trait('explosive cannon'),
    spell_save_dc(artificer, DC),
    format(string(Desc),
           "**Detonate.** Use an action when you are within 60 ft. of cannon. Destroys cannon; every creature within 20 ft. must make DC ~w DEX saving throw; on failure, they take 3d8 force damage, else half.\n\n",
           [DC]).

eldritch_cannon_note(_, Desc) :-
    trait('fortified position'),
    Desc = "**Fortified Position.** You and allies within 10 ft. of cannon have half cover.\n\n".

eldritch_cannon_note(_, "**Repairable.** If mending spell is cast on it, cannon is healed for 2d6 HP."). 
    
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
                   damage(Element, Dice + in_parens(1 d 8)), NewEffects).
apply_arcane_firearm(OldEffects, NewEffects) :-
    contains_multiple_damage_rolls(OldEffects),
    atomics_to_string(["+1d8 to one damage roll"], New),
    append(OldEffects, [New], NewEffects).
custom_format(modify_spell_field(effects, apply_arcane_firearm)) -->
    ["+1d8 to one damage roll."].

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

**Damage Immunities:** poison, psychic

",
        [HP]).

('eldritch cannon'(Type) ?= Str) :-
    eldritch_cannon_stat_block(StatBlock),
    eldritch_cannon_type(Type), % ground Type
    findall(Desc, eldritch_cannon_note(Type, Desc), Descs),
    atomics_to_string([StatBlock|Descs], Str).

'arcane firearm' ?= "At 5th level, You know how to turn a wand, staff, or rod into an arcane firearm, a conduit for your destructive spells. When you finish a long rest, you can use woodcarver's tools to carve special sigils into a wand, staff, or rod and thereby turn it into your arcane firearm. The sigils disappear from the object if you later carve them on a different item. The sigils otherwise last indefinitely.

You can use your arcane firearm as a spellcasting focus for your artificer spells. When you cast an artificer spell through the firearm, roll a d8, and you gain a bonus to one of the spell's damage rolls equal to the number rolled.".

'explosive cannon' ?= "Starting at 9th level, every eldritch cannon you create is more destructive:

- The cannon's damage rolls all increase by 1d8.

- As an action, you can command the cannon to detonate if you are within 60 feet of it. Doing so destroys the cannon and forces each creature within 20 feet of it to make a Dexterity saving throw against your spell save DC, taking 3d8 force damage on a failed save or half as much damage on a successful one.".

'fortified position' ?= "By 15th level, you’re a master at forming well-defended emplacements using Eldritch Cannon:

- You and your allies have half cover while within 10 feet of a cannon you create with Eldritch Cannon, as a result of a shimmering field of magical protection that the cannon emits.

- You can now have two cannons at the same time. You can create two with the same action (but not the same spell slot), and you can activate both of them with the same bonus action. You determine whether the cannons are identical to each other or different. You can't create a third cannon while you have two.".

