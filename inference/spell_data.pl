:- dynamic spell_auto_data/2.
:- multifile spell_auto_data/2, extend_spell_data/3.
:- discontiguous add_spell_effect/2, known_spell_effect/3.

:- [resources/spells/srd].

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

%! spell_base_damage_formula(?Spell, ?Damage)
%
%  Damage is a term `damage(Type, Formula)` that represents the damage
%  done by the Spell at the character's current level, without upcasting.
spell_base_damage_formula(Spell, damage(Type, N d D)) :-
    spell_property(Spell, damage_with_cantrip_scaling, damage(Type, _ d D)),
    cantrip_scale(N).
spell_base_damage_formula(Spell, Damage) :-
    spell_property(Spell, damage_at_slot_level, Dict),
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
    findall(Effect, add_spell_effect(Name, Effect), Effects).

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

%! add_spell_effect(?Name:atomic, ?Effect)
%
%  Add an entry to the `effects` field of the spell data.
add_spell_effect('acid splash', "up to two targets within 5 ft of eachother").
add_spell_effect('acid splash', saving_throw(dex): damage(acid, N d 6)) :-
    cantrip_scale(N).

add_spell_effect('burning hands',
                 in(15 ft cone):saving_throw(dex):(damage(fire, 3 d 6) else 'half damage')).

known_spell_effect(Origin, counterspell, Effect) :-
    known_spell(Origin, Ability, _, _, _, counterspell),
    atomics_to_string(
        ["target's spell fails if spell level not greater than 3, or if you pass a DC [10 + spell level] ",
         Ability,
         " check"],
        Effect).

add_spell_effect(darkvision, "grant 60 ft darkvision").

add_spell_effect('detect magic', "sense presence of magic within 30 ft").
add_spell_effect('detect magic', "use action to see faint aura around visible magical creature or object and learn its school of magic").
add_spell_effect('detect magic', "penetrate most barriers, but blocked by 1 ft stone, 1 inch common metal, thin sheet of lead, 3 ft wood or dirt").

add_spell_effect('eldritch blast', N*(spell_attack_roll(ranged):damage(force, 1 d 10))) :-
    cantrip_scale(N).

add_spell_effect('false life', 'temp hp' + (1 d 4 + 4)).

add_spell_effect('find familiar', test).

add_spell_effect(fireball,
                 in(20 ft sphere):
                  saving_throw(dex):
                   (damage(fire,8 d 6) else 'half damage')).

add_spell_effect('fire bolt',
                  spell_attack_roll(ranged):damage(fire, N d 10)) :-
    cantrip_scale(N).

add_spell_effect(frostbite,
                 saving_throw(con):damage(cold,N d 6)) :-
    cantrip_scale(N).

add_spell_effect('misty step', "teleport 30 ft").

%extend_spell_data('scorching ray', damage rolls, [on_hit: fire(2 d 6)]).
add_spell_effect('scorching ray',
                 3 * (spell_attack_roll(ranged):damage(fire, 2 d 6))).

add_spell_effect('see invisibility',
                 "see invisible creatures and objects, see through Ethereal").

add_spell_effect(shatter,
                 in(10 ft sphere):saving_throw(con):(damage(thunder, 3 d 8) else 'half damage')).
