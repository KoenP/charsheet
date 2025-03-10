class_option(druid).
hd_per_level(druid, 1 d 8).
initial_class_base_hp(druid, 8).
max_hp_per_level(druid, 1 d 8).
caster(druid, full).
spellcasting_ability(druid, wis).
max_prepared_spells(druid, N) :-
    default_max_prepared_spells(druid, N).
choose_subclass_level(druid:2).
asi_level(druid:L) :-
    default_asi_level(L).
class_saving_throw(druid, wis).
class_saving_throw(druid, int).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Initial class features.
class_skill_list(druid, [arcana, 'animal handling', insight, medicine,
                         nature, perception, religion, survival]).
traits_from_source(^druid,
                   [weapon(club), weapon(dagger), weapon(dart),
                    weapon(javelin), weapon(mace),
                    weapon(quarterstaff), weapon(scimitar),
                    weapon(sickle), weapon(sling), weapon(spear),
                    tool('herbalism kit')]).
trait_options_source(^druid, skill, wrap(skill),
                     2 unique_from class_skill(druid)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Features from leveling up.
traits_from_source(druid >: 1,
                   [armor(light), armor(medium), armor(shield),
                    language(druidic)]).
trait_source(druid >: 1, 'no metal armor').

trait_source(druid >: 2, wild_shape([cr(CR),hours(Hours)|Constraints])) :-
    wild_shape_cr(CR),
    class_level(druid:Level),
    Hours is floor(Level/2),
    findall(C, (wild_shape_constraint(L,C),Level<L), Constraints).
wild_shape_cr(CR) :-
    class_level(druid:Level),
    ordered_lookup_largest_leq([2 -> 1/4, 4 -> 1/2, 8 -> 1], Level, CR).
wild_shape_constraint(4, 'no swimming speed').
wild_shape_constraint(8, 'no flying speed').
res('wild shape', 2) :-
    trait(wild_shape(_)),
    \+ trait(archdruid).
restore_res('short rest', 'wild shape', 'full restore').

trait_source(druid >: 18, 'timeless body').
trait_source(druid >: 18, 'beast spells').

trait_source(druid >: 20, archdruid).
bonus(trait(archdruid), modify_spell(Druidic, _, archdruid_ignore_components)) :-
    trait(archdruid),
    Druidic =.. [druid|_].
archdruid_ignore_components(Data, Data.put(components,NewComponents)) :-
    findall(m(M),
            ( member(m(M), Data.get(components)),
              sub_string(M, _, _, _, "consumes"),
              sub_string(M, _, _, _, "gp")
            ),
            NewComponents).
meta_todo(archdruid, "implement more robust way to check material cost").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Spellcasting

% Druids get to pick cantrips.
known_spell(druid, wis, always, [], no, Spell) :-
    class_choice(druid, cantrip, Spell).
options_source(druid >: 1, cantrip, 2 unique_from class_cantrip(druid)).
options_source(druid >: L, cantrip, class_cantrip(druid)) :-
    L=4; L=10.

% Druids know all proper spells on their spell list.
% These need to be prepared to be cast and cost a spell slot to cast.
known_spell(druid, wis, 'when prepared', [slot], Ritual, Spell) :-
    learnable_proper_spell(druid, Spell),
    \+ (trait(circle_spells(Circle)), druid_circle_spell(Circle, Spell)),
    spell_property(Spell, ritual, Ritual).

% Circle spells are always prepared.
known_spell(druid, wis, always, [slot], Ritual, Spell) :-
    trait(circle_spells(Circle)),
    druid_circle_spell(Circle, Spell),
    learnable_proper_spell(druid, Spell),
    spell_property(Spell, ritual, Ritual).

% Add circle spells to druid spell list.
extend_class_spell_list(druid, Spell) :-
    subclass(Class),
    Class =.. [druid, Circle],
    druid_circle_spell(Circle, Spell).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% SUBCLASSES (DRUIDIC CIRCLES)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- discontiguous druid_circle_spell/2.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Circle of the land
subclass_option(druid, land).
custom_format(druid(land)) --> ["druid (circle of the land)"].
options_source(druid(land) >: 2, cantrip, class_cantrip(druid)).
trait_source(druid(land) >: 2, natural_recovery(Total)) :-
    class_level(druid:L),
    Total is ceiling(L / 2).
res('natural recovery', Slots) :-
    trait(natural_recovery(Slots)).
restore_res('long rest', 'natural recovery', 'full restore').

trait_options_source(
    druid(land) >: 3,
    'circle spells',
    wrap(circle_spells),
    from_list([arctic,coast,desert,forest,grassland,mountain,swamp])).

trait_source(druid(land) >: 6, 'land\'s stride').

trait_source(druid(land) >: 10, 'nature\'s ward').

trait_source(druid(land) >: 14, 'nature\'s sanctuary').

druid_circle_spell(arctic, 'hold person').
druid_circle_spell(arctic, 'spike growth').
druid_circle_spell(arctic, 'sleet storm').
druid_circle_spell(arctic, slow).
druid_circle_spell(arctic, 'freedom of movement').
druid_circle_spell(arctic, 'ice storm').
druid_circle_spell(arctic, 'commune with nature').
druid_circle_spell(arctic, 'cone of cold').

druid_circle_spell(coast, 'mirror image').
druid_circle_spell(coast, 'misty step').
druid_circle_spell(coast, 'water breathing').
druid_circle_spell(coast, 'water walk').
druid_circle_spell(coast, 'control water').
druid_circle_spell(coast, 'freedom of movement').
druid_circle_spell(coast, 'conjure elemental').
druid_circle_spell(coast, scrying).

druid_circle_spell(desert, blur).
druid_circle_spell(desert, silence).
druid_circle_spell(desert, 'create food and water').
druid_circle_spell(desert, 'protection from energy').
druid_circle_spell(desert, blight).
druid_circle_spell(desert, 'hallucinatory terrain').
druid_circle_spell(desert, 'insect plague').
druid_circle_spell(desert, 'wall of stone').

druid_circle_spell(forest, barkskin).
druid_circle_spell(forest, 'spider climb').
druid_circle_spell(forest, 'call lightning').
druid_circle_spell(forest, 'plant growth').
druid_circle_spell(forest, divination).
druid_circle_spell(forest, 'freedom of movement').
druid_circle_spell(forest, 'commune with nature').
druid_circle_spell(forest, 'tree stride').

druid_circle_spell(grassland, invisibility).
druid_circle_spell(grassland, 'pass without trace').
druid_circle_spell(grassland, daylight).
druid_circle_spell(grassland, haste).
druid_circle_spell(grassland, divination).
druid_circle_spell(grassland, 'freedom of movement').
druid_circle_spell(grassland, dream).
druid_circle_spell(grassland, 'insect plague').

druid_circle_spell(mountain, 'spider climb').
druid_circle_spell(mountain, 'spike growth').
druid_circle_spell(mountain, 'lightning bolt').
druid_circle_spell(mountain, 'meld into stone').
druid_circle_spell(mountain, 'stone shape').
druid_circle_spell(mountain, stoneskin).
druid_circle_spell(mountain, passwall).
druid_circle_spell(mountain, 'wall of stone').

druid_circle_spell(swamp, 'acid arrow').
druid_circle_spell(swamp, darkness).
druid_circle_spell(swamp, 'water walk').
druid_circle_spell(swamp, 'stinking cloud').
druid_circle_spell(swamp, 'freedom of movement').
druid_circle_spell(swamp, 'locate creature').
druid_circle_spell(swamp, 'insect plague').
druid_circle_spell(swamp, scrying).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Circle of spores
subclass_option(druid, spores).
custom_format(druid(spores)) --> ["druid (circle of spores)"].

trait_source(druid(spores) >: 2, circle_spells(spores)).

known_spell(druid, wis, always, [], no, 'chill touch') :-
    druid(spores) >: 2.
druid_circle_spell(spores, 'blindness/deafness').
druid_circle_spell(spores, 'gentle repose').
druid_circle_spell(spores, 'animate dead').
druid_circle_spell(spores, 'gaseous form').
druid_circle_spell(spores, blight).
druid_circle_spell(spores, confusion).
druid_circle_spell(spores, cloudkill).
druid_circle_spell(spores, contagion).

trait_source(druid(spores) >: 2, halo_of_spores(1 d D)) :-
    class_level(druid:Level),
    ordered_lookup_largest_leq([2 -> 4, 6 -> 6, 10 -> 8, 14 -> 10], Level, D).

trait_source(druid(spores) >: 2, 'symbiotic entity').

trait_source(druid(spores) >: 6, 'fungal infestation').
res('fungal infestation', Uses) :-
    trait('fungal infestation'),
    ability_mod(wis, WisMod),
    Uses is max(WisMod, 1).
restore_res('long rest', 'fungal infestation', 'full restore').

trait_source(druid(spores) >: 10, 'spreading spores').

trait_source(druid(spores) >: 14, 'fungal body').

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
language(druidic) ?= "You know Druidic, the secret language of druids. You can speak the language and use it to leave hidden messages. You and others who know this language automatically spot such a message. Others spot the message's presence with a successful DC 15 Wisdom (Perception) check but can't decipher it without magic.".

wild_shape(_) ?= ["Starting at 2nd level, you can use your action to magically assume the shape of a beast that you have seen before. You can use this feature twice. You regain expended uses when you finish a short or long rest.
Your druid level determines the beasts you can transform into, as shown in the Beast Shapes table. At 2nd level, for example, you can transform into any beast that has a challenge rating of 1/4 or lower that doesn't have a flying or swimming speed.
Beast Shapes

| Level | Max. CR | Limitations                 | Example     |
|-------|---------|-----------------------------|-------------|
| 2nd   | 1/4     | No flying or swimming speed | Wolf        |
| 4th   | 1/2     | No flying speed             | Crocodile   |
| 8th   | 1       | -                           | Giant Eagle |

You can stay in a beast shape for a number of hours equal to half your druid level (rounded down). You then revert to your normal form unless you expend another use of this feature. You can revert to your normal form earlier by using a bonus action on your turn. You automatically revert if you fall unconscious, drop to 0 hit points, or die.
While you are transformed, the following rules apply:",

"- Your game statistics are replaced by the statistics of the beast, but you retain your alignment, personality, and Intelligence, Wisdom, and Charisma scores. You also retain all of your skill and saving throw proficiencies, in addition to gaining those of the creature. If the creature has the same proficiency as you and the bonus in its stat block is higher than yours, use the creature's bonus instead of yours. If the creature has any legendary or lair actions, you can't use them.
- When you transform, you assume the beast's hit points and Hit Dice. When you revert to your normal form, you return to the number of hit points you had before you transformed. However, if you revert as a result of dropping to 0 hit points, any excess damage carries over to your normal form. For example, if you take 10 damage in animal form and have only 1 hit point left, you revert and take 9 damage. As long as the excess damage doesn't reduce your normal form to 0 hit points, you aren't knocked unconscious.
- You can't cast spells, and your ability to speak or take any action that requires hands is limited to the capabilities of your beast form. Transforming doesn't break your concentration on a spell you've already cast, however, or prevent you from taking actions that are part of a spell, such as call lightning, that you've already cast.
- You retain the benefit of any features from your class, race, or other source and can use them if the new form is physically capable of doing so. However, you can't use any of your special senses, such as darkvision, unless your new form also has that sense.
- You choose whether your equipment falls to the ground in your space, merges into your new form, or is worn by it. Worn equipment functions as normal, but the GM decides whether it is practical for the new form to wear a piece of equipment, based on the creature's shape and size. Your equipment doesn't change size or shape to match the new form, and any equipment that the new form can't wear must either fall to the ground or merge with it. Equipment that merges with the form has no effect until you leave the form.
"].

'timeless body' ?= "Starting at 18th level, the primal magic that you wield causes you to age more slowly. For every 10 years that pass, your body ages only 1 year.".

'beast spells' ?= "Beginning at 18th level, you can cast many of your druid spells in any shape you assume using Wild Shape. You can perform the somatic and verbal components of a druid spell while in a beast shape, but you aren't able to provide material components.".

archdruid ?= "At 20th level, you can use your Wild Shape an unlimited number of times.
Additionally, you can ignore the verbal and somatic components of your druid spells, as well as any material components that lack a cost and aren't consumed by a spell. You gain this benefit in both your normal shape and your beast shape from Wild Shape.".

'natural recovery' ?= "Starting at 2nd level, you can regain some of your magical energy by sitting in meditation and communing with nature. During a short rest, you choose expended spell slots to recover. The spell slots can have a combined level that is equal to or less than half your druid level (rounded up), and none of the slots can be 6th level or higher. You can't use this feature again until you finish a long rest.
For example, when you are a 4th-level druid, you can recover up to two levels worth of spell slots. You can recover either a 2nd-level slot or two 1st-level slots.".

'land\'s stride' ?= "Starting at 6th level, moving through nonmagical difficult terrain costs you no extra movement. You can also pass through nonmagical plants without being slowed by them and without taking damage from them if they have thorns, spines, or a similar hazard.
In addition, you have advantage on saving throws against plants that are magically created or manipulated to impede movement, such those created by the entangle spell.".

'nature\'s ward' ?= "When you reach 10th level, you can't be charmed or frightened by elementals or fey, and you are immune to poison and disease.".

'nature\'s sanctuary' ?= "When you reach 14th level, creatures of the natural world sense your connection to nature and become hesitant to attack you. When a beast or plant creature attacks you, that creature must make a Wisdom saving throw against your druid spell save DC. On a failed save, the creature must choose a different target, or the attack automatically misses. On a successful save, the creature is immune to this effect for 24 hours. The creature is aware of this effect before it makes its attack against you.".

% Cirlce of spores.
halo_of_spores(_) ?= "You are surrounded by invisible, necrotic spores that are harmless until you unleash them on a creature nearby. When a creature you can see moves into a space within 10 feet of you or starts its turn there, you can use your reaction to deal 1d4 necrotic damage to that creature unless it succeeds on a Constitution saving throw against your spell save DC. The necrotic damage increases to 1d6 at 6th level, 1d8 at 10th level, and 1d10 at 14th level.".

'symbiotic entity' ?= "You gain the ability to channel magic into your spores. As an action, you can expend a use of your Wild Shape feature to awaken those spores, rather than transforming into a beast form, and you gain 4 temporary hit points for each level you have in this class. While this feature is active, you gain the following benefits:

- When you deal your Halo of Spores damage, roll the damage die a second time and add it to the total.
- Your melee weapon attacks deal an extra 1d6 necrotic damage to any target they hit.

These benefits last for 10 minutes, until you lose all these temporary hit points, or until you use your Wild Shape again.".

'fungal infestation' ?= "Your spores gain the ability to infest a corpse and animate it. If a beast or a humanoid that is Small or Medium dies within 10 feet of you, you can use your reaction to animate it, causing it to stand up immediately with 1 hit point. The creature uses the zombie stat block in the Monster Manual. It remains animate for 1 hour, after which time it collapses and dies.

In combat, the zombie’s turn comes immediately after yours. It obeys your mental commands, and the only action it can take is the Attack action, making one melee attack.

You can use this feature a number of times equal to your Wisdom modifier (minimum of once), and you regain all expended uses of it when you finish a long rest.".

'spreading spores' ?= "You gain the ability to seed an area with deadly spores. As a bonus action while your Symbiotic Entity feature is active, you can hurl spores up to 30 feet away, where they swirl in a 10-foot cube for 1 minute. The spores disappear early if you use this feature again, if you dismiss them as a bonus action, or if your Symbiotic Entity feature is no longer active.

Whenever a creature moves into the cube or starts its turn there, that creature takes your Halo of Spores damage, unless the creature succeeds on a Constitution saving throw against your spell save DC. A creature can take this damage no more than once per turn.

While the cube of spores persists, you can’t use your Halo of Spores reaction.".

'fungal body' ?= "The fungal spores in your body alter you: you can’t be blinded, deafened, frightened, or poisoned, and any critical hit against you counts as a normal hit instead, unless you’re incapacitated.".

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

language(druidic)@=srd(66).
wild_shape(_)@=srd('66-67').
'beast spells'@=srd('67').
archdruid@=srd('67-68').
natural_recovery(_)@=srd('68').
'nature\'s ward'@=srd('69').
'nature\'s sanctuary'@=srd('69').
circle_spells(spores)@=phb('68').
