:- use_module(library(http/json)).

register_srd_spells :-
    open('inference/resources/spells/srd.json', read, In),
    json:json_read_dict(In, SpellDicts),
    maplist(spell_data_damage_with_cantrip_scaling, SpellDicts, Out).

register_spell(Data) :-
    to_lowercase_atom(Data.name, Name),
    spell_data_higher_level(Data.higher_level, HigherLevel),
    spell_data_components(Data, Components),
    parse_range(Data.range, Range),
    maplist(spell_data_class, Data.classes, Classes),
    spell_data_damage_with_cantrip_scaling(Data, DamageCantripScaling),
    assert(spell_auto_data(Name,
                           properties{ level: Data.level,
                                       higher_level: HigherLevel,          
                                       school: Data.school.index,
                                       components: Components,
                                       range: Range,
                                       casting_time: Data.casting_time,
                                       duration: Data.duration,
                                       concentration: Data.concentration,
                                       ritual: Data.ritual,
                                       desc: Data.desc,
                                       classes: Classes,
                                       damage_with_cantrip_scaling: DamageCantripScaling
                                     })).

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

% TODO hier zat ik: bug bij acid splash
spell_data_damage_with_cantrip_scaling(Data, damage(Type, BaseRoll)) :-
    DmgDict = Data.get(damage),
    DmgScalingDict = DmgDict.get(damage_at_character_level),
    to_lowercase_atom(DmgDict.get(damage_type).get(name), Type),
    parse_roll(DmgScalingDict.get(1), BaseRoll).
spell_data_damage_with_cantrip_scaling(Data, false) :-
    \+ (DmgDict = Data.get(damage), DmgDict.get(damage_at_character_level)).

%spell_data_damage_list(Data, Damage) :-
%    wrap_list(Data.get(damage), DamageDicts),
%    DamageDicts = [DmgDict|_],
%    spell_data_damage_scaling_type_and_range(DmgDict, ScalingType, )
%    maplist(spell_data_damage, DamageDicts, Damage).
%
%spell_data_damage(DamageDict, damage(Type, Term)) :-
%    to_lowercase_atom(DamageDict.damage_type.name, Type),
%    spell_data_damage_scaling_type_and_range(DamageDict, ScalingType, Low-High)
%
%spell_data_damage_scaling_type_and_range(Dict, ScalingType, Low-High) :-
%    (ScalingType = damage_at_slot_level; ScalingType = damage_at_character_level),
%    dict_keys(Dict.get(ScalingType), Keys),
%    Keys = [Low|_],
%    last(Keys, High).

parse_roll(String, Roll) :-
    string_chars(String, Chars),
    maplist(try_atom_number, Chars, CharNumbers),
    phrase(format_dice(CharNumbers), Roll).

try_atom_number(Atom, Number) :-
    atom_number(Atom, Number),
    !.
try_atom_number(Atom, Atom).


% Complications:
% - Damage at slot level vs at character level
% - Multiple damage types (for example 'flame strike')
% - Add modifier ("+ MOD")

% :- register_srd_spells.
