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
    string_to_atom(Data.school.index, School),
    spell_data_components(Data, Components),
    parse_range(Data.range, Range),
    yesno(Data.concentration, Concentration),
    yesno(Data.ritual, Ritual),
    maplist(spell_data_class, Data.classes, Classes),
    spell_data_damage_with_cantrip_scaling(Data, DamageCantripScaling),
    spell_data_damage_at_slot_level(Data, DamageSlotLevel),
    spell_data_aoe(Data, AOE),
    spell_data_dc(Data, DC),
    spell_data_attack_type(Data, AttackType),
    get_or_default(Data, material, false, Material),
    assert(spell_auto_data(Name,
                           properties{ level: Data.level,
                                       higher_level: HigherLevel,          
                                       school: School,
                                       components: Components,
                                       range: Range,
                                       casting_time: Data.casting_time,
                                       duration: Data.duration,
                                       concentration: Concentration,
                                       ritual: Ritual,
                                       desc: Data.desc,
                                       classes: Classes,
                                       damage_with_cantrip_scaling: DamageCantripScaling,
                                       damage_at_slot_level: DamageSlotLevel,
                                       area_of_effect: AOE,
                                       dc: DC,
                                       attack_type: AttackType,
                                       material: Material
                                     })).

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

:- \+ spell_auto_data(_,_) -> register_srd_spells; true.
