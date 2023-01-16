:- use_module(library(http/json)).

raw_data(SpellName, Data) :-
    open('inference/resources/spells/srd.json', read, In),
    json:json_read_dict(In, SpellDicts),
    member(Data, SpellDicts),
    to_lowercase_atom(Data.name, SpellName).

register_srd_spells :-
    open('inference/resources/spells/srd.json', read, In),
    json:json_read_dict(In, SpellDicts),
    maplist(register_spell, SpellDicts).

register_spell(Data) :-
    to_lowercase_atom(Data.name, Name),
    spell_data_higher_level(Data.higher_level, HigherLevel),
    spell_data_components(Data, Components),
    parse_range(Data.range, Range),
    maplist(spell_data_class, Data.classes, Classes),
    spell_data_damage_with_cantrip_scaling(Data, DamageCantripScaling),
    spell_data_damage_at_slot_level(Data, DamageSlotLevel),
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
                                       damage_with_cantrip_scaling: DamageCantripScaling,
                                       damage_at_slot_level: DamageSlotLevel
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
            (member(LvlAtom-RollStr,Pairs), atom_number(LvlAtom,Lvl), term_string(Roll,RollStr)),
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

:- register_srd_spells.
