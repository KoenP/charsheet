:- [druid/spells].

class_option(druid).
caster(druid, full). % druids are "full casters"
hd_per_level(druid, 1 d 8).
initial_class_base_hp(druid, 8).
max_hp_per_level(druid, 1 d 8).
class_saving_throw(druid, int).
class_saving_throw(druid, wis).
choose_subclass_level(druid, 2).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Generic features and options available on druid level 1.
class_trait(druid:1, armor('light armor'  ) ).
class_trait(druid:1, armor('medium armor' ) ).
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
class_trait(druid:1, tool('herbalism kit') ).
class_trait(druid:1, language(druidic)).
class_trait(druid:1, ritual_casting(druid)).
class_trait(druid:1, spellcasting_focus(druid)).
language(druidic) ?= "You know Druidic, the Secret language of druids. You can speak the language and use it to leave hidden messages. You and others who know this language automatically spot such a Message. Others spot the message's presence with a successful DC 15 Wisdom (Perception) check but can't decipher it without magic.".

wrap_class_trait_option(druid:1, skill, X, skill(X)).
class_trait_options(druid:1, skill, 2 from Proficiencies) :-
    Proficiencies = [ arcana , 'animal handling' , insight , medicine , nature
                    , perception , religion , survival].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Spellcasting.

% Cantrips.
wrap_class_trait_option(druid:_, cantrip, X, learn_spell(druid, X)).
class_trait_options(druid:1, cantrip, 2 from Cantrips) :-
    findall(Cantrip, class_cantrip(druid, Cantrip), Cantrips).
class_trait_options(druid:Level, cantrip, 1 from Cantrips) :-
    member(Level, [4, 10]),
    findall(Cantrip, class_cantrip(druid, Cantrip), Cantrips).

% Druids "know" (= can prepare) all non-cantrip druid spells for which
% they have slots.
% We want to avoid duplicating the spells we know from druid circle though,
% and we don't want to duplicate 'alter self' for a level 14 and above
% moon druid.
spell_known(SpellName, druid, wis, when_prepared, spell_slot) :-
    spell_learnable(druid, SpellName),
    spell(SpellName, level, Level),
    \+ (SpellName = 'alter self', trait('thousand forms')),
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
class_trait(druid:2, 'wild shape').
summary('wild shape') --> ['CR '], format_ws_cr, format_ws_restrictions.
%custom_display_rule('wild shape', Display) :-
%    phrase(display_wild_shape, Atomics),
%    atomics_to_string(Atomics, Display).

format_ws_cr --> {wild_shape_max_cr(CR)}, display_cr(CR).
format_ws_restrictions --> {findall(R,wild_shape_restriction(R),Rs), length(Rs,Len), length(Commas,Len), maplist(=(', '),Commas)},
                         interleave(Commas, Rs).
%sep([','], Rs).

display_cr(CR) --> display_fraction(CR).
display_cr(CR) --> {number(CR)}, [CR].

display_fraction(N / D) --> [N], ['/'], [D].
%restrictions --> restrictions0.
%restrictions0 --> , [','], [R], restrictions0.


'wild shape' ?= "Starting at 2nd level, you can use your action to magically assume the shape of a beast that you have seen before. You can use this feature twice. You regain expended uses when you finish a short or long rest.
Your druid level determines the beasts you can transform into, as shown in Table: Beast Shapes. At 2nd level, for example, you can transform into any beast that has a challenge rating of 1/4 or lower that doesn’t have a flying or swimming speed.
You can stay in a beast shape for a number of hours equal to half your druid level (rounded down). You then revert to your normal form unless you expend another use of this feature. You can revert to your normal form earlier by using a bonus action on your turn. You automatically revert if you fall unconscious, drop to 0 hit points, or die.
While you are transformed, the following rules apply:
Your game statistics are replaced by the statistics of the beast, but you retain your alignment, personality, and Intelligence, Wisdom, and Charisma scores. You also retain all of your skill and saving throw proficiencies, in addition to gaining those of the creature. If the creature has the same proficiency as you and the bonus in its stat block is higher than yours, use the creature’s bonus instead of yours. If the creature has any legendary or lair actions, you can’t use them.
When you transform, you assume the beast’s hit points and Hit Dice. When you revert to your normal form, you return to the number of hit points you had before you transformed. However, if you revert as a result of dropping to 0 hit points, any excess damage carries over to your normal form. For example, if you take 10 damage in animal form and have only 1 hit point left, you revert and take 9 damage. As long as the excess damage doesn’t reduce your normal form to 0 hit points, you aren’t knocked unconscious.
You can’t cast spells, and your ability to speak or take any action that requires hands is limited to the capabilities of your beast form. Transforming doesn’t break your concentration on a spell you’ve already cast, however, or prevent you from taking actions that are part of a spell, such as call lightning, that you’ve already cast.
You retain the benefit of any features from your class, race, or other source and can use them if the new form is physically capable of doing so. However, you can’t use any of your special senses, such as darkvision, unless your new form also has that sense.
You choose whether your equipment falls to the ground in your space, merges into your new form, or is worn by it. Worn equipment functions as normal, but the GM decides whether it is practical for the new form to wear a piece of equipment, based on the creature’s shape and size. Your equipment doesn’t change size or shape to match the new form, and any equipment that the new form can’t wear must either fall to the ground or merge with it. Equipment that merges with the form has no effect until you leave the form.".

resource('wild shape', Uses) :-
    wild_shape_uses(Uses).
on_short_rest('wild shape', restore) :-
    trait('wild shape').

wild_shape_uses(2) :-
    class(druid),
    \+ trait(archdruid).
wild_shape_uses(unlimited) :-
    trait(archdruid).

wild_shape_max_cr(CR) :-
    class_level(druid:Level),
    \+ trait(subclass(druid:_, moon), 'circle forms'),
    ( between(2,3,Level) -> CR = 1 / 4
    ; between(4,7,Level) -> CR = 1 / 2
    ; Level >= 8 -> CR = 1).
wild_shape_max_cr(CR) :-
    class_level(druid:Level),
    trait(subclass(druid:_, moon), 'circle forms'),
    moon_max_cr(Level, CR).
moon_max_cr(Level, 1) :-
    between(2, 5, Level).
moon_max_cr(Level, CR) :-
    between(6, 20, Level),
    CR is floor(Level / 3).

wild_shape_restriction('no flying') :-
    class_level(druid:Level),
    Level < 8.
wild_shape_restriction('no swimming') :-
    class_level(druid:Level),
    Level < 4.

%custom_section(Table) :-
%    trait('wild shape'),
%    table('wild shape', "Wild shape", [Header,Row], Table),
%    Header = tr([th('Uses left'), th('Max CR'), th('Restrictions')]),
%    Row = tr([td(Boxes), td(CR), td(Restrictions)]),
%    wild_shape_uses(Uses), repl(input(type=checkbox,[]), Uses, Boxes),
%    wild_shape_max_cr(CRVal), display_term(CRVal, CR),
%    findall(R, wild_shape_restriction(R), Restrictions).
    

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Remaining general features.
class_trait(druid:18, timeless_body).
class_trait(druid:18, beast_spells).
class_trait(druid:20, archdruid).

timeless_body ?= "Starting at 18th level, the primal magic that you wield causes you to age more slowly. For every 10 years that pass, your body ages only 1 year.".
beast_spells ?= "Beginning at 18th level, you can cast many of your druid spells in any shape you assume using Wild Shape. You can perform the somatic and verbal components of a druid spell while in a beast shape, but you aren’t able to provide material components.".
archdruid ?= "At 20th level, you can use your Wild Shape an unlimited number of times. Additionally, you can ignore the verbal and somatic components of your druid spells, as well as any material components that lack a cost and aren’t consumed by a spell. You gain this benefit in both your normal shape and your beast shape from Wild Shape.".

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Subclass: Circle of the Moon
subclass_option(druid, moon).
subclass_trait(druid:2, moon, 'combat wild shape').
subclass_trait(druid:2, moon, 'circle forms').
subclass_trait(druid:6, moon,  'primal strike').
subclass_trait(druid:10, moon, 'elemental wild shape').
subclass_trait(druid:14, moon, 'thousand forms').
spell_known('alter self', druid, wis, always_available, at_will) :-
    trait('thousand forms').

'combat wild shape' ?= "When you choose this circle at 2nd level, you gain the ability to use Wild Shape on your turn as a bonus action, rather than as an action.

Additionally, while you are transformed by Wild Shape, you can use a bonus action to expend one spell slot to regain 1d8 hit points per level of the spell slot expended.".
'circle forms' ?= "The rites of your circle grant you the ability to transform into more dangerous animal forms. Starting at 2nd level, you can use your Wild Shape to transform into a beast with a challenge rating as high as 1. You ignore the Max. CR column of the Beast Shapes table, but must abide by the other limitations there.

Starting at 6th level, you can transform into a beast with a challenge rating as high as your druid level divided by 3, rounded down.".

'primal strike' ?= "Starting at 6th level, your attacks in beast form count as magical for the purpose of overcoming resistance and immunity to nonmagical attacks and damage.".
'elemental wild shape' ?= "At 10th level, you can expend two uses of Wild Shape at the same time to transform into an air elemental, an earth elemental, a fire elemental, or a water elemental.".
'thousand forms' ?= "By 14th level, you have learned to use magic to alter your physical form in more subtle ways. You can cast the Alter Self spell at will.".

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Subclass: Circle of the Land
subclass_option(druid, land).

subclass_trait_options(druid:2, land, extra_cantrip, 1 from Cantrips) :-
    findall(learn_spell(druid, Cantrip), class_cantrip(druid, Cantrip), Cantrips).

% TODO add short rest effect.
subclass_trait(druid:2, land, 'natural recovery').
'natural recovery' ?= "Starting at 2nd level, you can regain some of your magical energy by sitting in meditation and communing with nature. During a short rest, you choose expended spell slots to recover. The spell slots can have a combined level that is equal to or less than half your druid level (rounded up), and none of the slots can be 6th level or higher. You can’t use this feature again until you finish a long rest.

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
