:- use_module(library(http/json)).

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
                                       classes: Classes
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

:- register_srd_spells.
