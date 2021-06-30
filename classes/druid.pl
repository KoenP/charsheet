class_option(druid).
hd_per_level(druid, 1 d 8).
initial_class_base_hp(druid, 8).
max_hp_per_level(druid, 1 d 8).
saving_throw(druid, int).
saving_throw(druid, wis).

gain_spell_slots(druid, spell_level(1), [1,1,2,3]).
gain_spell_slots(druid, spell_level(2), [3,3,4]).
gain_spell_slots(druid, spell_level(3), [5,5,6]).
gain_spell_slots(druid, spell_level(4), [7,8,9]).
gain_spell_slots(druid, spell_level(5), [7,8,9]).
gain_spell_slots(druid, spell_level(6), [11,19]).
gain_spell_slots(druid, spell_level(7), [13,20]).
gain_spell_slots(druid, spell_level(8), [15]).
gain_spell_slots(druid, spell_level(9), [17]).

% Features and options available on druid level 1.
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
class_trait(druid:1, spellcasting(wis)).
class_trait(druid:1, ritual_casting(druid)).
class_trait(druid:1, spellcasting_focus(druid)).
describe(language(druidic), "You know Druidic, the Secret language of druids. You can speak the language and use it to leave hidden messages. You and others who know this language automatically spot such a Message. Others spot the message's presence with a successful DC 15 Wisdom (Perception) check but can't decipher it without magic.").

class_trait_options(druid:1, druid_skills, 2, Proficiency) :-
    member(Proficiency, [ proficient(arcana)
                        , proficient(animal_handling)
                        , proficient(insight)
                        , proficient(medicine)
                        , proficient(nature)
                        , proficient(perception)
                        , proficient(religion)
                        , proficient(survival)
                        ]).

% TODO hier zat ik.

%feature_options(class(druid), druid_skills, 2, [ proficient(arcana)
%                                               , proficient(animal_handling)
%                                               , proficient(insight)
%                                               , proficient(medicine)
%                                               , proficient(nature)
%                                               , proficient(perception)
%                                               , proficient(religion)
%                                               , proficient(survival)
%                                               ]).

class_trait_options(druid:1, druid_cantrips, 2, Cantrip) :-
    class_cantrip(druid, Cantrip).

% Features and options unlocked by leveling up the druid class.
% TODO how to incorporate the wild shape rules.
class_trait(druid:2, wild_shape).
describe(wild_shape, "Starting at 2nd level, you can use your action to magically assume the shape of a beast that you have seen before. You can use this feature twice. You regain expended uses when you finish a short or long rest.
Your druid level determines the beasts you can transform into, as shown in Table: Beast Shapes. At 2nd level, for example, you can transform into any beast that has a challenge rating of 1/4 or lower that doesn’t have a flying or swimming speed.
You can stay in a beast shape for a number of hours equal to half your druid level (rounded down). You then revert to your normal form unless you expend another use of this feature. You can revert to your normal form earlier by using a bonus action on your turn. You automatically revert if you fall unconscious, drop to 0 hit points, or die.
While you are transformed, the following rules apply:").
describe(wild_shape, "Your game statistics are replaced by the statistics of the beast, but you retain your alignment, personality, and Intelligence, Wisdom, and Charisma scores. You also retain all of your skill and saving throw proficiencies, in addition to gaining those of the creature. If the creature has the same proficiency as you and the bonus in its stat block is higher than yours, use the creature’s bonus instead of yours. If the creature has any legendary or lair actions, you can’t use them.").
describe(wild_shape, "When you transform, you assume the beast’s hit points and Hit Dice. When you revert to your normal form, you return to the number of hit points you had before you transformed. However, if you revert as a result of dropping to 0 hit points, any excess damage carries over to your normal form. For example, if you take 10 damage in animal form and have only 1 hit point left, you revert and take 9 damage. As long as the excess damage doesn’t reduce your normal form to 0 hit points, you aren’t knocked unconscious.").
describe(wild_shape, "You can’t cast spells, and your ability to speak or take any action that requires hands is limited to the capabilities of your beast form. Transforming doesn’t break your concentration on a spell you’ve already cast, however, or prevent you from taking actions that are part of a spell, such as call lightning, that you’ve already cast.").
describe(wild_shape, "You retain the benefit of any features from your class, race, or other source and can use them if the new form is physically capable of doing so. However, you can’t use any of your special senses, such as darkvision, unless your new form also has that sense.").
describe(wild_shape, "You choose whether your equipment falls to the ground in your space, merges into your new form, or is worn by it. Worn equipment functions as normal, but the GM decides whether it is practical for the new form to wear a piece of equipment, based on the creature’s shape and size. Your equipment doesn’t change size or shape to match the new form, and any equipment that the new form can’t wear must either fall to the ground or merge with it. Equipment that merges with the form has no effect until you leave the form.").

%feature_options(class(druid,4), ability_score_improvement, 2, Options) :-
%    findall(add_ability(Ability,1), ability(Ability), Options).
class_trait_options(druid:4, druid_cantrips, 1, Cantrip) :-
    class_cantrip(druid, Cantrip).
