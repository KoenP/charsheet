:- [druid/spells].

class_option(druid).
hd_per_level(druid, 1 d 8).
initial_class_base_hp(druid, 8).
max_hp_per_level(druid, 1 d 8).
class_saving_throw(druid, int).
class_saving_throw(druid, wis).
choose_subclass_level(druid, 2).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Generic features and options available on druid level 1.
class_trait(druid:1, armor(light_armor  ) ).
class_trait(druid:1, armor(medium_armor ) ).
class_trait(druid:1, armor(shields      ) ).
class_trait(druid:1, weapon(club        ) ).
class_trait(druid:1, weapon(dagger      ) ).
class_trait(druid:1, weapon(dart        ) ).
class_trait(druid:1, weapon(javelin     ) ).
class_trait(druid:1, weapon(mace        ) ).
class_trait(druid:1, weapon(quarterstaff) ).
class_trait(druid:1, weapon(scimitar    ) ).
class_trait(druid:1, weapon(sickle      ) ).
class_trait(druid:1, weapon(sling       ) ).
class_trait(druid:1, weapon(spear       ) ).
class_trait(druid:1, tool(herbalism_kit) ).
class_trait(druid:1, language(druidic)).
class_trait(druid:1, ritual_casting(druid)).
class_trait(druid:1, spellcasting_focus(druid)).
language(druidic) ?= "You know Druidic, the Secret language of druids. You can speak the language and use it to leave hidden messages. You and others who know this language automatically spot such a Message. Others spot the message's presence with a successful DC 15 Wisdom (Perception) check but can't decipher it without magic.".

class_trait_options(druid:1, skills, 2 from Proficiencies) :-
    Proficiencies = [ skill(arcana)
                    , skill('animal handling')
                    , skill(insight)
                    , skill(medicine)
                    , skill(nature)
                    , skill(perception)
                    , skill(religion)
                    , skill(survival)
                    ].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Spellcasting.

% Cantrips.
class_trait_options(druid:1, cantrip, 2 from Cantrips) :-
    findall(learn_spell(druid, Cantrip), class_cantrip(druid, Cantrip), Cantrips).
class_trait_options(druid:Level, cantrip, 1 from Cantrips) :-
    member(Level, [4, 10]),
    findall(learn_spell(druid, Cantrip), class_cantrip(druid, Cantrip), Cantrips).

% Spell slot table.
gain_spell_slots(druid, spell_level(1), [1,1,2,3]).
gain_spell_slots(druid, spell_level(2), [3,3,4]).
gain_spell_slots(druid, spell_level(3), [5,5,6]).
gain_spell_slots(druid, spell_level(4), [7,8,9]).
gain_spell_slots(druid, spell_level(5), [7,8,9]).
gain_spell_slots(druid, spell_level(6), [11,19]).
gain_spell_slots(druid, spell_level(7), [13,20]).
gain_spell_slots(druid, spell_level(8), [15]).
gain_spell_slots(druid, spell_level(9), [17]).

% Druids "know" (= can prepare) all non-cantrip druid spells for which
% they have slots.
% We want to avoid duplicating the spells we know from druid circle though.
spell_known(SpellName, druid, wis, when_prepared, spell_slot) :-
    spell_learnable(druid, SpellName),
    spell(SpellName, level, Level),
    \+ trait(learn_circle_spell(SpellName)),
    Level > 0.

% Calculate how many spells a druid can prepare.
max_prepared_spells(druid, N) :-
    ability_mod(wis, WisMod),
    class_level(druid:Level),
    N is Level + WisMod.

% Druids use wisdom as their spellcasting ability.
spellcasting_ability(druid, wis).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Wild shape
class_trait(druid:2, wild_shape).
% TODO wild shape CR etc
wild_shape ?= "Starting at 2nd level, you can use your action to magically assume the shape of a beast that you have seen before. You can use this feature twice. You regain expended uses when you finish a short or long rest.
Your druid level determines the beasts you can transform into, as shown in Table: Beast Shapes. At 2nd level, for example, you can transform into any beast that has a challenge rating of 1/4 or lower that doesn’t have a flying or swimming speed.
You can stay in a beast shape for a number of hours equal to half your druid level (rounded down). You then revert to your normal form unless you expend another use of this feature. You can revert to your normal form earlier by using a bonus action on your turn. You automatically revert if you fall unconscious, drop to 0 hit points, or die.
While you are transformed, the following rules apply:".
wild_shape ?= "Your game statistics are replaced by the statistics of the beast, but you retain your alignment, personality, and Intelligence, Wisdom, and Charisma scores. You also retain all of your skill and saving throw proficiencies, in addition to gaining those of the creature. If the creature has the same proficiency as you and the bonus in its stat block is higher than yours, use the creature’s bonus instead of yours. If the creature has any legendary or lair actions, you can’t use them.".
wild_shape ?= "When you transform, you assume the beast’s hit points and Hit Dice. When you revert to your normal form, you return to the number of hit points you had before you transformed. However, if you revert as a result of dropping to 0 hit points, any excess damage carries over to your normal form. For example, if you take 10 damage in animal form and have only 1 hit point left, you revert and take 9 damage. As long as the excess damage doesn’t reduce your normal form to 0 hit points, you aren’t knocked unconscious.".
wild_shape ?= "You can’t cast spells, and your ability to speak or take any action that requires hands is limited to the capabilities of your beast form. Transforming doesn’t break your concentration on a spell you’ve already cast, however, or prevent you from taking actions that are part of a spell, such as call lightning, that you’ve already cast.".
wild_shape ?= "You retain the benefit of any features from your class, race, or other source and can use them if the new form is physically capable of doing so. However, you can’t use any of your special senses, such as darkvision, unless your new form also has that sense.".
wild_shape ?= "You choose whether your equipment falls to the ground in your space, merges into your new form, or is worn by it. Worn equipment functions as normal, but the GM decides whether it is practical for the new form to wear a piece of equipment, based on the creature’s shape and size. Your equipment doesn’t change size or shape to match the new form, and any equipment that the new form can’t wear must either fall to the ground or merge with it. Equipment that merges with the form has no effect until you leave the form.".

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Remaining general features.
class_trait(druid:18, timeless_body).
class_trait(druid:18, beast_spells).
class_trait(druid:20, archdruid).

timeless_body ?= "Starting at 18th level, the primal magic that you wield causes you to age more slowly. For every 10 years that pass, your body ages only 1 year.".
beast_spells ?= "Beginning at 18th level, you can cast many of your druid spells in any shape you assume using Wild Shape. You can perform the somatic and verbal components of a druid spell while in a beast shape, but you aren’t able to provide material components.".
archdruid ?= "At 20th level, you can use your Wild Shape an unlimited number of times. Additionally, you can ignore the verbal and somatic components of your druid spells, as well as any material components that lack a cost and aren’t consumed by a spell. You gain this benefit in both your normal shape and your beast shape from Wild Shape.".

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Subclass: Circle of the Land
subclass_option(druid, land).

subclass_trait_options(druid:2, land, extra_cantrip, 1 from Cantrips) :-
    findall(learn_spell(druid, Cantrip), class_cantrip(druid, Cantrip), Cantrips).

% TODO add short rest effect.
subclass_trait(druid:2, land, natural_recovery).
natural_recovery ?= "Starting at 2nd level, you can regain some of your magical energy by sitting in meditation and communing with nature. During a short rest, you choose expended spell slots to recover. The spell slots can have a combined level that is equal to or less than half your druid level (rounded up), and none of the slots can be 6th level or higher. You can’t use this feature again until you finish a long rest.

For example, when you are a 4th-level druid, you can recover up to two levels worth of spell slots. You can recover either a 2nd-level slot or two 1st-level slots.".

% Learn circle spells.
subclass_trait_options(druid:3, land, land_type, 1 from Types) :-
    findall(druid_land_type(Type), druid_land_type_option(Type), Types).

subclass_trait_options(druid:Level, land, circle_spell, 1 from Spells) :-
    member(Level, [3,5,7,9]),
    druid_land_type(Type),
    druid_circle_spells_at_level(Type, Level, Spells).

spell_known(Spell, druid, wis, always_available, spell_slot) :-
    trait(learn_circle_spell(Spell)).

% Land's stride.
subclass_trait(druid:6, land, lands_stride).
lands_stride ?= "Starting at 6th level, moving through nonmagical difficult terrain costs you no extra movement. You can also pass through nonmagical plants without being slowed by them and without taking damage from them if they have thorns, spines, or a similar hazard.

In addition, you have advantage on saving throws against plants that are magically created or manipulated to impede movement, such those created by the entangle spell.".

subclass_trait(druid:10, land, natures_ward).
natures_ward ?= "When you reach 10th level, you can’t be charmed or frightened by elementals or fey, and you are immune to poison and disease.".

subclass_trait(druid:14, land, natures_sanctuary).
natures_sanctuary ?= "When you reach 14th level, creatures of the natural world sense your connection to nature and become hesitant to attack you. When a beast or plant creature attacks you, that creature must make a Wisdom saving throw against your druid spell save DC. On a failed save, the creature must choose a different target, or the attack automatically misses. On a successful save, the creature is immune to this effect for 24 hours.".

druid_land_type(Type) :-
    trait(choose_traits(subclass(druid:3, land), land_type), druid_land_type(Type)).

druid_land_type_option(arctic).
druid_land_type_option(coastal).
druid_land_type_option(desert).
druid_land_type_option(forest).
druid_land_type_option(grassland).
druid_land_type_option(mountain).
druid_land_type_option(swamp).

druid_circle_spells_at_level(arctic, 3, [learn_circle_spell('hold person'),
                                         learn_circle_spell('spike growth')]).
druid_circle_spells_at_level(arctic, 5, [learn_circle_spell('sleet storm'),
                                         learn_circle_spell(slow)]).
druid_circle_spells_at_level(arctic, 7, [learn_circle_spell('freedom of movement'),
                                         learn_circle_spell('ice storm')]).
druid_circle_spells_at_level(arctic, 9, [learn_circle_spell('commune with nature'),
                                         learn_circle_spell('cone of cold')]).
