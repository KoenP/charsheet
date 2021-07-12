% Standalone script to convert the json spell list from
% https://github.com/jcquinlan/dnd-spells/blob/master/spells.json
% into prolog facts.

:- use_module(library(http/json)).

read_spells(Spells) :-
    open('resources/spells.json', read, In),
    json:json_read_dict(In, SpellsJson),
    maplist(transform_dict, SpellsJson, Spells),
    close(In).

write_spells(Spells) :-
    open('spells/spells.pl', write, Out),
    writeln(Out, "% Auto generated, do not edit directly.\n"),
    maplist(write_spell(Out), Spells).

write_spell(Out, Spell) :-
    write_term(Out, Spell, [quoted(true), nl(true), fullstop(true)]),
    writeln(Out, "").
    
transform_dict(In, spell(Name, properties{ level:Level,
                                           higher_level:HigherLevel,
                                           school:School,
                                           components:Components,
                                           range:Range,
                                           casting_time:In.casting_time,
                                           duration:In.duration,
                                           ritual:Ritual,
                                           desc:In.desc
                                         })) :-
    to_lowercase_atom(In.name, Name),
    parse_level(In.level, Level),
    get_or_default(In, higher_level, no, HigherLevel),
    to_lowercase_atom(In.school, School),
    get_or_default(In, material, "err: no material", Material),
    parse_components(In.components, Material, Components),
    parse_range(In.range, Range),
    string_to_atom(In.ritual, Ritual).

to_lowercase_atom(Str, Atom) :-
    string_lower(Str, Lower),
    string_to_atom(Lower, Atom).

get_or_default(Dict, Field, _, Dict.get(Field)) :-
    !.
get_or_default(Dict, Field, Default, Default) :-
    \+ (_ = Dict.get(Field)).


parse_level("Cantrip", 0).
parse_level(Str, Level) :-
    member(Suffix, ["th-level", "st-level", "nd-level", "rd-level"]),
    string_concat(N, Suffix, Str),
    !,
    number_string(Level, N).

parse_components(Str, Material, Components) :-
    string_chars(Str, Chars),
    split_on([',',' '], Chars, Comps),
    maplist(process_component(Material), Comps, Components).
process_component(Material, ['M'], m(Material)).
process_component(_, [C], C2) :-
    C \= 'M',
    to_lower(C,C1),
    char_code(C2,C1).

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

split_on(Sep, List, [List]) :-
    \+ (append(_, Rest, List), append(Sep, _, Rest)).
split_on(Sep, List, [X|Xs]) :-
    append(Sep, AfterSep, Tail),
    append(X, Tail, List),
    !,
    split_on(Sep, AfterSep, Xs).

:- read_spells(Spells),
   write_spells(Spells).

