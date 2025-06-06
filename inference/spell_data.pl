%:- [resources/spells/srd].
:- [spell_auto_data].
:- table spell_data/2.
:- multifile extend_spell_data/3.
:- multifile add_spell_effect/2, known_spell_effect/3, suppress_autoderived_spell_effect/1, seminotable/1.

%! spell_property(?Name:atomic, ?Field:atomic, ?Val)
%
%  Retrieve a specific Field from the spell_data/2 of the spell Name.
spell_property(Name, Field, Val) :-
    spell_data(Name, Data),
    Val = Data.get(Field).

%! spell_property_or_error(?Name:atomic, ?Prop:atomic, ?Val)
%
%  Like spell_property/3, but when the given field does not exist, it
%  throws an error instead of silently failing.
spell_property_or_error(Name, Prop, Val) :-
    spell_data(Name, Data),
    Val = Data.Prop.

%! spell_data(?Name:atomic, ?Data:dict)
%
%  Data is a dictionary with the following fields (optional fields in parens):
%  * casting_time: A string describing the time required to cast the spell.
%  * classes: List of classes (atoms) that can learn the spell.
%  * components: Subset of the set [`v`, `s`, `m(X)`], indicating
%      which components are required.
%  * desc: Textual description.
%  * duration: String describing the amount of time the effect lasts.
%  * higher_level: Atom `yes` or `no`, indicating whether the spell can be upcast.
%  * level: Spell level, as an integer.
%  * range: Description of the range of a spell. Some options are
%      `self`, `touch`, `feet(X)`, `miles(X)`.
%  * ritual: `yes` or `no`, depending on whether the spell can be cast as a ritual.
%  * school: The spell's school (as an atom).
%  * (damage_rolls): Information about the damage rolls performed by this spell.
%      The damage information is recorded as terms of the form `Cond: Type(Roll)`
%      where `Cond` is a condition that needs to be met to apply the roll, for
%      example `on_hit` (if the damage is done if a spell attack roll
%      hits), `Type` is the damage type (piercing, bludgeoning, fire,
%      ...), and `Roll` is a dice formula.
%  * (effects): Summary of effects, but not damage rolls.
spell_data(Name, Data) :-
    spell_auto_data(Name, AutoData),
    findall(Ext,
            (extend_spell_data(Name, Field, Val), Ext=add_dict_field(Field:Val)),
            Exts),
    sequence(Exts, AutoData, Data).
add_dict_field(Field:Val, Old, New) :-
    New = Old.put(Field,Val).

desc_paragraphs(Ps, Text) :-
    intersperse("\n\n", Ps, Interspersed),
    atomics_to_string(Interspersed, Text).
intersperse(_, [], []).
intersperse(_, [Y], [Y]).
intersperse(X, [Y|Ys], [Y,X|Zs]) :-
    Ys = [_|_],
    intersperse(X, Ys, Zs).



%! spell_effect(?Spell, ?Effect)
spell_effect(Spell, AutoEffect) :-
    autoderived_spell_effect(Spell, AutoEffect),
    \+ suppress_autoderived_spell_effect(Spell).
spell_effect(Spell, Effect) :-
    add_spell_effect(Spell, Effect).

%spell_effects(Spell, Effects) :-
%    suppress_autoderived_spell_effect(Spell),
%    findall(E, add_spell_effect(Spell, E), Effects).
%spell_effects(Spell, [AutoEffect|AddedEffects]) :-
%    spell_auto_data(Spell, _),
%    \+ suppress_autoderived_spell_effect(Spell),
%    autoderived_spell_effect(Spell, AutoEffect),
%    findall(E, add_spell_effect(Spell, E), AddedEffects).
    

%! autoderived_spell_effect(?Spell, ?Effect)
%
%  For each Spell, Effect represents a best-effort attempt to derive a
%  structured spell description from the JSON sources. It's not
%  guaranteed to be correct and can be suppressed by
%  suppress_autoderived_spell_effect/1.
autoderived_spell_effect(Spell, Effect) :-
    spell_auto_data(Spell, Data),
    spell_base_damage_formula(Spell, Damage),
    findall(Field-Value,
            (member(Field, [area_of_effect, dc, attack_type]),
             Data.get(Field) = Value,
             Value \= false),
            Aspects_),
    append(Aspects_, [damage - Damage], Aspects),
    process_spell_aspects(Aspects, Effect).
process_spell_aspects([area_of_effect - (Range ft Shape) | Aspects], in(Range ft Shape) : Effect) :-
    process_spell_aspects(Aspects, Effect).
process_spell_aspects([dc - (Abi else Success) | Aspects], saving_throw(Abi) : EffectExpr) :-
    (Success = none -> EffectExpr = Effect ; EffectExpr = (Effect else Success)),
    process_spell_aspects(Aspects, Effect).
process_spell_aspects([attack_type - Type | Aspects], spell_attack_roll(Type) : Effect) :-
    process_spell_aspects(Aspects, Effect).
process_spell_aspects([damage - Damage], Damage).

%! spell_base_damage_formula(?Spell, ?Damage)
%
%  Damage is a term `damage(Type, Formula)` that represents the damage
%  done by the Spell at the character's current level, without
%  upcasting or any character-specific bonuses (other than character
%  level).
spell_base_damage_formula(Spell, damage(Type, N d D)) :-
    spell_auto_property(Spell, damage_with_cantrip_scaling, damage(Type, _ d D)),
    cantrip_scale(N).
spell_base_damage_formula(Spell, Damage) :-
    spell_auto_property(Spell, damage_at_slot_level, Dict),
    dict_pairs(Dict, _, [_-Damage|_]).

%! spell_auto_data(?Name:atomic, ?Data:dict)
%
%  Like spell_data/2, but less complete. This is the data that is
%  autogenerated from a JSON file, whereas spell_data/2 contains
%  further manual extensions. You probably want spell_data/2.

%! contains_attack_roll(+Effects, ?Roll)
%
%  Check whether Effects contains at least one attack roll.
contains_attack_roll(Effects, spell_attack_roll(Range):Effect) :-
    subterm_member(spell_attack_roll(Range):Effect, Effects).

%! contains_saving_throw(+Effects, ?ST)
%
%  Check whether Effects contains at least one saving throw.
contains_saving_throw(Effects, saving_throw(Abi):Effect) :-
    subterm_member(saving_throw(Abi):Effect, Effects).

%! unique_effects(+Effects, ?UniqueEffects)
%
%  Effects may contain terms of the form `N * Effect`, where `N` is a
%  natural number. For each such term in Effects where `N=1`,
%  UniqueEffects contains simply Effect.
unique_effects(Effects, UniqueEffects) :-
    maplist(just_once, Effects, UniqueEffects).
just_once(_*X, X).
just_once(X, X) :- X \= _*_.

%! extend_spell_data(+Name:atomic, ?Field:atomic, ?Val)
extend_spell_data(Name, effects, Effects) :-
    spell_auto_data(Name, _), % ground Name
    findall(Effect, spell_effect(Name, Effect), Effects).

%! known_spell_effect(?Origin, ?Name:atomic, ?Effect)
%
%  Like add_spell_effect/2, but only gets applied to spells your
%  character knows, typically because some class-specific data is
%  needed to generate the effect summary for the spell. An example of
%  this is counterspell, where in some cases the caster needs to roll
%  an ability check with their own spellcasting ability. Filling in
%  this spellcasting ability requires knowing as which class the spell
%  has been learned.
known_spell_effect(_,_,_) :- false.

known_spell_effect(Origin, counterspell, Effect) :-
    known_spell(Origin, Ability, _, _, _, counterspell),
    atomics_to_string(
        ["target's spell fails if spell level not greater than 3, or if you pass a DC [10 + spell level] ",
         Ability,
         " check"],
        Effect).

%! add_spell_effect(?Name:atomic, ?Effect)
%
%  Add an entry to the `effects` field of the spell data.
add_spell_effect('acid splash', "up to two targets within 5 ft of eachother").

add_spell_effect(bless, "bless up to 3 creatures").
add_spell_effect(bless, "+d4 to attack rolls / saving throws").

add_spell_effect(command, "command creature to approach, drop, flee, grovel, or halt").

add_spell_effect('cure wounds', heal(1 d 8 + mod)).

add_spell_effect(darkvision, "grant 60 ft darkvision").

add_spell_effect('detect magic', "sense presence of magic within 30 ft").

suppress_autoderived_spell_effect('eldritch blast').
add_spell_effect('eldritch blast', Effect) :-
    cantrip_scale(N),
    simplify_product(N*(spell_attack_roll(ranged):damage(force, 1 d 10)), Effect),
    !.

add_spell_effect('false life', 'temp hp' + (1 d 4 + 4)).

% Find familiar.
trait_source(known_spell(Source, 'find familiar'), familiar_option(Source, Fam)) :-
    familiar_option(Source, Fam).
seminotable(familiar_option(_,_)).
familiar_option(Source, Fam) :-
    known_spell(Source, 'find familiar'),
    member(Fam, [bat, cat, crab, frog, toad, hawk, lizard, octopus, owl, 'poisonous snake', fish, rat, raven, 'sea horse', spider, weasel]).
familiar_option(Source, Fam) :-
    bonus(extra_familiar_option(Source, Fam)).
(familiar_option(Source, Fam) ?= Desc) :-
    creature(Fam, Dict),
    amend_creature_for_find_familiar(Source, Dict, FamDict),
    creature_dict_to_desc(Fam, FamDict, Desc).
custom_format(familiar_option(_, Fam)) --> [Fam], [" familiar"].

amend_creature_for_find_familiar(Source, Old, New) :-
    bonus(override_amend_creature_for_find_familiar(Source, Amend)),
    call(Amend, Old, New),
    !.
amend_creature_for_find_familiar(_, Old, New) :-
    compose([creature_dict_remove_attack_actions,
             creature_dict_add_no_attack_note,
             creature_dict_add_deliver_touch_spell_reaction
            ],
            Old,
            New).
creature_dict_remove_attack_actions(Old, New) :-
    get_dict(actions, Old, OldActions, New, NewActions),
    dict_pairs(OldActions, _, OldPairs),
    findall(Tag-Act,
            (member(Tag-Act, OldPairs), \+ (is_dict(Act, attack))),
            NewPairs),
    dict_pairs(NewActions, _, NewPairs).
creature_dict_add_no_attack_note(Old, New) :-
    Tag = 'no attack',
    Note = "Cannot take the attack action.",
    get_dict(traits, Old, OldTraits, New, NewTraits),
    NewTraits = OldTraits.put(Tag, Note).
creature_dict_add_deliver_touch_spell_reaction(Old, New) :-
    Tag = 'deliver spell',
    Note = "If you cast a spell with range of touch and familiar is within 100 ft., it can deliver the spell as a reaction. If the spell requires an attack roll, use your action modifier.",
    get_dict(actions, Old, OldActions, New, NewActions),
    NewActions = OldActions.put(Tag, Note).
% ..

add_spell_effect(frostbite,
                 saving_throw(con):damage(cold,N d 6)) :-
    cantrip_scale(N).

add_spell_effect(guidance, "+d4 to one ability check").

add_spell_effect('healing word', heal(1 d 4 + mod)).

bonus_source(known_spell(_,'mage armor'), ac_formula(13 + dex + shield)).

add_spell_effect('mass cure wounds', heal(3 d 8 + mod) upto "6 creatures").

add_spell_effect('misty step', "teleport 30 ft").

add_spell_effect('prayer of healing', heal(2 d 8 + mod) upto "6 creatures").

%extend_spell_data('scorching ray', damage rolls, [on_hit: fire(2 d 6)]).
suppress_autoderived_spell_effect('scorching ray').
add_spell_effect('scorching ray', 3 * ADEffect) :-
    autoderived_spell_effect('scorching ray', ADEffect).

add_spell_effect('see invisibility',
                 "see invisible creatures and objects, see through Ethereal").

attack_variant(Name:shillelagh, Range, to_hit(ToHit), [damage(bludgeoning, 1 d 8 + Mod)], [magical]) :-
    (Name = quarterstaff ; Name = club),
    attack(Name, Range, _, _, _),
    known_spell(_, shillelagh),
    ability_mod(wis, Mod),
    proficiency_bonus(ProfBon),
    ToHit is Mod + ProfBon.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% SHORTENED DESCRIPTIONS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
spell_short_desc(
    'prismatic wall',
    "Creates a multicolored, opaque wall up to 90 feet long, 30 feet high, and 1 inch thick, or a spherical wall up to 30 feet in diameter. If the wall passes through a creature's space, the spell fails. Creatures designated by the caster can pass through without harm. Creatures within 20 feet of the wall at the start of their turn must make a Constitution saving throw or be blinded for 1 minute. The wall sheds bright light for 100 feet and dim light for an additional 100 feet. Consists of seven layers, each with a different color.

1. **Red**: Deals 10d6 fire damage. Nonmagical ranged attacks can't pass through.
2. **Orange**: Deals 10d6 acid damage. Magical ranged attacks can't pass through. Destroyed by strong wind.
3. **Yellow**: Deals 10d6 lightning damage. Can be destroyed by dealing 60 force damage.
4. **Green**: Deals 10d6 poison damage. Can be destroyed by a passwall spell or similar.
5. **Blue**: Deals 10d6 cold damage. Can be destroyed by dealing 25 fire damage.
6. **Indigo**: Restrains creatures; requires saving throws to avoid petrification. Spells can't pass through. Destroyed by bright light (e.g., daylight spell).
7. **Violet**: Blinds creatures, transports them to another plane on failed saves. Destroyed by dispel magic or similar spells.

Layers can be destroyed one at a time, from red to violet, by specific means outlined for each layer. Rod of cancellation destroys the entire wall. Antimagic field has no effect on the wall.").


spell_short_desc(
    symbol,
    "The spell allows you to inscribe a harmful glyph on a surface or within a closed object. The glyph is nearly invisible and can be triggered by various means that you decide upon casting. If the trigger occurs, the glyph activates, filling a 60-foot-radius sphere with dim light for 10 minutes. Creatures in the sphere during activation or entering it are affected based on the chosen glyph effect:",
      "Death: Targets make a Constitution saving throw, taking 10d10 necrotic damage on a failed save, or half as much on a successful save.",
      "Discord: Targets make a Constitution saving throw. On a failed save, they bicker and argue for 1 minute, with disadvantage on attack rolls and ability checks.",
      "Fear: Targets make a Wisdom saving throw. On a failed save, they become frightened for 1 minute, dropping held items and moving away from the glyph.",
      "Hopelessness: Targets make a Charisma saving throw. On a failed save, they are overwhelmed with despair for 1 minute, unable to attack or target creatures with harmful effects.",
      "Insanity: Targets make an Intelligence saving throw. On a failed save, they are insane for 1 minute, unable to take actions, understand speech, read, and move erratically under DM control.",
      "Pain: Targets make a Constitution saving throw. On a failed save, they are incapacitated with excruciating pain for 1 minute.",
      "Sleep: Targets make a Wisdom saving throw. On a failed save, they fall unconscious for 10 minutes, waking up if damaged or shaken awake.",
      "Stunning: Targets make a Wisdom saving throw. On a failed save, they become stunned for 1 minute.").

spell_short_desc(
    teleport,
    "This spell instantly transports you and up to eight willing creatures or a single object to a selected destination within range on the same plane of existence.",

      "| Familiarity | Mishap | Similar Area | Off Target | On Target |\n |-------------|--------|--------------|------------|-----------|\n | Permanent Circle | - | - | - | 01-100 |\n | Associated Object | - | - | - | 01-100 |\n | Very Familiar | 01-05 | 06-13 | 14-24 | 25-100 |\n | Seen Casually | 01-33 | 34-43 | 44-53 | 54-100 |\n | Viewed Once | 01-43 | 44-53 | 54-73 | 74-100 |\n | Description | 01-43 | 44-53 | 54-73 | 74-100 |\n | False Destination | 01-50 | 51-100 | - | - |",

      "**Outcomes**",
      "- **On Target:** Successful teleportation to the desired destination.",
      "- **Off Target:** Random distance (1d10 x 1d10 % of travelled distance) away from the destination in a random direction.",
      "- **Similar Area:** Arrival in an area visually or thematically similar to the target.",
      "- **Mishap:** Unpredictable magic causes a difficult journey, dealing 3d10 force damage, and DM rerolls on the table."
).

spell_short_desc(
    imprisonment,
    "Target must succeed on a Wisdom saving throw or be bound by the spell. If it succeeds, it is immune to this spell if you cast it again. While affected by this spell, target doesn't need to breathe, eat, or drink, and doesn't age. Divination spells can't locate or perceive the target.",
      "Choose a form of imprisonment:",
      "Burial: Force sphere far beneath the earth. Nothing can pass through; no one can teleport / planar travel into or out of the sphere. Component: small mithral orb.",
      "Chaining: Target is restrained, and it can't move or be moved. Component: fine chain of precious metal.",
      "Hedged Prison: Tiny demiplane that is warded against teleportation and planar travel. Component: miniature representation of the prison made from jade.",
      "Minimus Containment: Target shrinks to a height of 1 inch and is imprisoned inside a gemstone or similar object. Light can pass through the gemstone normally (allowing the target to see out and other creatures to see in), but nothing else can pass through, even by means of teleportation or planar travel. The gemstone can't be cut or broken while the spell remains in effect. Component: transparent gemstone.",
      "Slumber: Target falls asleep and can't be awoken. Component: rare soporific herbs.",
      "Ending the Spell: While casting spell, you can specify a condition that ends the spell.",
      "A dispel magic spell can end the spell only if it is cast as a 9th-level spell, targeting either the prison or the special component used to create it.",
      "You can use a particular special component to create only one prison at a time. If you cast the spell again using the same component, the target of the first casting is immediately freed from its binding."
).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% TODO copied from srd.pl
spell_auto_property(Spell, Field, Value) :-
    spell_auto_data(Spell, Data),
    Data.get(Field) = Value,
    Value \= false.

spell_data_class(Dict, Class) :-
    to_lowercase_atom(Dict.index, Class).

spell_data_higher_level([], no).
spell_data_higher_level([Desc], Desc).

spell_data_components(Data, Components) :-
    maplist(spell_data_component(Data), Data.components, Components).

spell_data_component(Data, "M", m(Data.material)) :- !.
spell_data_component(_, Component, Atom) :-
    to_lowercase_atom(Component, Atom).

to_lowercase_atom(Str, Atom) :-
    string_lower(Str, Lower),
    string_to_atom(Lower, Atom).

yesno(true, yes).
yesno(false, no).

parse_range("Self", self) :- !.
parse_range("Touch", touch) :- !.
parse_range(Str, feet(Feet)) :-
    member(Suffix, [" feet", " foot"]),
    string_concat(FeetStr, Suffix, Str),
    number_string(Feet, FeetStr),
    !.
parse_range(Str, miles(Miles)) :-
    member(Suffix, [" mile", " miles"]),
    string_concat(MilesStr, Suffix, Str),
    number_string(Miles, MilesStr),
    !.
parse_range(Str, Atom) :-
    to_lowercase_atom(Str, Atom).

wrap_list(List, List) :- is_list(List), !.
wrap_list(X, [X]) :- \+ is_list(X).

spell_data_damage_with_cantrip_scaling(Data, damage(Type, BaseRoll)) :-
    Data.get(damage) = _{ damage_at_character_level: DmgScalingDict, 
                          damage_type: DmgType },
    to_lowercase_atom(DmgType.get(name), Type),
    term_string(BaseRoll, DmgScalingDict.get('1')),
    !.
spell_data_damage_with_cantrip_scaling(_, false) :- !.

spell_data_damage_at_slot_level(Data, ParsedDict) :-
    wrap_list(Data.get(damage), DamageDicts),
    maplist(damage_at_slot_level_term, DamageDicts, Terms),
    merge_damage_dicts(Terms, ParsedDict),
    !.
spell_data_damage_at_slot_level(_, []).

damage_at_slot_level_term(_{ damage_type: TypeDict,
                             damage_at_slot_level: ScalingDict
                           },
                          ParsedDict) :-
    to_lowercase_atom(TypeDict.get(name), Type),
    dict_pairs(ScalingDict, _, Pairs),
    findall(Lvl-damage(Type, Roll),
            (member(LvlAtom-RollStr,Pairs), atom_number(LvlAtom,Lvl), term_string(Roll,RollStr,[variable_names([])])), % TODO: parse and handle "+ MOD"
            NewPairs),
    dict_pairs(ParsedDict, _, NewPairs).

merge_damage_dicts([D|Ds], Out) :-
    merge_damage_dicts(Ds, DRest),
    merge_damage_dicts(D, DRest, Out).
merge_damage_dicts([D], D).
merge_damage_dicts(D1, D2, Out) :-
    dict_pairs(D1, _, Pairs1), dict_pairs(D2, _, Pairs2),
    merge_damage_lists(Pairs1, Pairs2, NewPairs),
    dict_pairs(Out, _, NewPairs).
merge_damage_lists([L-Dmg1|R1], [L-Dmg2|R2], [L-(Dmg1+Dmg2)|R]) :-
    merge_damage_lists(R1, R2, R).
merge_damage_lists([], [], []).

% in(20 ft sphere):
spell_data_aoe(Data, Size ft Type) :-
    Data.get(area_of_effect) = _{type: TypeStr, size: Size},
    !,
    string_to_atom(TypeStr, Type).
spell_data_aoe(_, false).

spell_data_dc(Data, Abi else Succ) :-
    Data.get(dc) = DCDict,
    !,
    DCDict.get(dc_type).get(index) = AbiStr,
    string_to_atom(AbiStr, Abi),
    DCDict.get(dc_success) = SuccStr,
    string_to_atom(SuccStr, Succ).
spell_data_dc(_, false).

spell_data_attack_type(Data, Type) :-
    Data.get(attack_type) = Str,
    string_to_atom(Str, Type). 
spell_data_attack_type(_, false).
