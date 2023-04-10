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
traits_from_source(initial_class(druid),
                   [weapon(club), weapon(dagger), weapon(dart),
                    weapon(javelin), weapon(mace),
                    weapon(quarterstaff), weapon(scimitar),
                    weapon(sickle), weapon(sling), weapon(spear),
                    tool('herbalism kit')]).
trait_options_source(initial_class(druid), skill, wrap(skill),
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
resource('wild shape', 'wild shape', 2) :-
    trait(wild_shape(_)),
    \+ trait(archdruid).
on_rest(short, 'wild shape', full_restore).

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
options_source(class(druid), cantrip, 2 unique_from class_cantrip(druid)).
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
subclass_option(druid, land).
custom_format(druid(land)) --> ["druid (circle of the land)"].
options_source(druid(land) >: 2, cantrip, class_cantrip(druid)).
trait_source(druid(land) >: 2, natural_recovery(Total)) :-
    class_level(druid:L),
    Total is ceiling(L / 2).
resource('natural recovery', 'spell slot total', Slots) :-
    trait(natural_recovery(Slots)).
on_rest(long, 'natural recovery', full_restore).

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
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

'no metal armor' ?= "druids will not wear armor or use shields made of metal".
language(druidic) ?= "You know Druidic, the secret language of druids. You can speak the language and use it to leave hidden messages. You and others who know this language automatically spot such a message. Others spot the message's presence with a successful DC 15 Wisdom (Perception) check but can't decipher it without magic.".

wild_shape(_) ?= "Starting at 2nd level, you can use your action to magically assume the shape of a beast that you have seen before. You can use this feature twice. You regain expended uses when you finish a short or long rest.
Your druid level determines the beasts you can transform into, as shown in the Beast Shapes table. At 2nd level, for example, you can transform into any beast that has a challenge rating of 1/4 or lower that doesn't have a flying or swimming speed.
Beast Shapes
Level 	Max. CR 	Limitations 	Example
2nd 	1/4 	No flying or swimming speed 	Wolf
4th 	1/2 	No flying speed 	Crocodile
8th 	1 	- 	Giant Eagle

You can stay in a beast shape for a number of hours equal to half your druid level (rounded down). You then revert to your normal form unless you expend another use of this feature. You can revert to your normal form earlier by using a bonus action on your turn. You automatically revert if you fall unconscious, drop to 0 hit points, or die.
While you are transformed, the following rules apply:

    Your game statistics are replaced by the statistics of the beast, but you retain your alignment, personality, and Intelligence, Wisdom, and Charisma scores. You also retain all of your skill and saving throw proficiencies, in addition to gaining those of the creature. If the creature has the same proficiency as you and the bonus in its stat block is higher than yours, use the creature's bonus instead of yours. If the creature has any legendary or lair actions, you can't use them.
    When you transform, you assume the beast's hit points and Hit Dice. When you revert to your normal form, you return to the number of hit points you had before you transformed. However, if you revert as a result of dropping to 0 hit points, any excess damage carries over to your normal form. For example, if you take 10 damage in animal form and have only 1 hit point left, you revert and take 9 damage. As long as the excess damage doesn't reduce your normal form to 0 hit points, you aren't knocked unconscious.
    You can't cast spells, and your ability to speak or take any action that requires hands is limited to the capabilities of your beast form. Transforming doesn't break your concentration on a spell you've already cast, however, or prevent you from taking actions that are part of a spell, such as call lightning, that you've already cast.
    You retain the benefit of any features from your class, race, or other source and can use them if the new form is physically capable of doing so. However, you can't use any of your special senses, such as darkvision, unless your new form also has that sense.
    You choose whether your equipment falls to the ground in your space, merges into your new form, or is worn by it. Worn equipment functions as normal, but the GM decides whether it is practical for the new form to wear a piece of equipment, based on the creature's shape and size. Your equipment doesn't change size or shape to match the new form, and any equipment that the new form can't wear must either fall to the ground or merge with it. Equipment that merges with the form has no effect until you leave the form.
".

'timeless body' ?= "Starting at 18th level, the primal magic that you wield causes you to age more slowly. For every 10 years that pass, your body ages only 1 year.".

'beast spells' ?= "Beginning at 18th level, you can cast many of your druid spells in any shape you assume using Wild Shape. You can perform the somatic and verbal components of a druid spell while in a beast shape, but you aren't able to provide material components.".

archdruid ?= "At 20th level, you can use your Wild Shape an unlimited number of times.
Additionally, you can ignore the verbal and somatic components of your druid spells, as well as any material components that lack a cost and aren't consumed by a spell. You gain this benefit in both your normal shape and your beast shape from Wild Shape.".

'natural recovery' ?= "Starting at 2nd level, you can regain some of your magical energy by sitting in meditation and communing with nature. During a short rest, you choose expended spell slots to recover. The spell slots can have a combined level that is equal to or less than half your druid level (rounded up), and none of the slots can be 6th level or higher. You can't use this feature again until you finish a long rest.
For example, when you are a 4th-level druid, you can recover up to two levels worth of spell slots. You can recover either a 2nd-level slot or two 1st-level slots.".

'circle spells' ?= "Your mystical connection to the land infuses you with the ability to cast certain spells. At 3rd, 5th, 7th, and 9th level you gain access to circle spells connected to the land where you became a druid. Choose that land - arctic, coast, desert, forest, grassland, mountain, or swamp - and consult the associated list of spells.
Once you gain access to a circle spell, you always have it prepared, and it doesn't count against the number of spells you can prepare each day. If you gain access to a spell that doesn’t appear on the druid spell list, the spell is nonetheless a druid spell for you.".

'land\'s stride' ?= "Starting at 6th level, moving through nonmagical difficult terrain costs you no extra movement. You can also pass through nonmagical plants without being slowed by them and without taking damage from them if they have thorns, spines, or a similar hazard.
In addition, you have advantage on saving throws against plants that are magically created or manipulated to impede movement, such those created by the entangle spell.".

'nature\'s ward' ?= "When you reach 10th level, you can't be charmed or frightened by elementals or fey, and you are immune to poison and disease.".

'nature\'s sanctuary' ?= "When you reach 14th level, creatures of the natural world sense your connection to nature and become hesitant to attack you. When a beast or plant creature attacks you, that creature must make a Wisdom saving throw against your druid spell save DC. On a failed save, the creature must choose a different target, or the attack automatically misses. On a successful save, the creature is immune to this effect for 24 hours. The creature is aware of this effect before it makes its attack against you.".
