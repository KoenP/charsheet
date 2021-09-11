:- [spell_auto_data].

%! spell_property(?Name:atomic, ?Prop:atomic, ?Val)
spell_property(Name, Prop, Val) :-
    spell_data(Name, Data),
    Val = Data.get(Prop).

%! spell_property_or_error(?Name:atomic, ?Prop:atomic, ?Val)
spell_property_or_error(Name, Prop, Val) :-
    spell_data(Name, Data),
    Val = Data.Prop.

%! spell_data(?Name:atomic, ?Data:dict)
spell_data(Name, Data) :-
    spell_auto_data(Name, AutoData),
    findall(Ext,
            (extend_spell_data(Name, Field, Val), Ext=add_dict_field(Field:Val)),
            Exts),
    sequence(Exts, AutoData, Data).
add_dict_field(Field:Val, Old, New) :-
    New = Old.put(Field,Val).

%! extend_spell_data(?Name:atomic, ?Field:atomic, ?Val)
extend_spell_data('fire bolt', damage_rolls, [on_hit(fire(N d 10))]) :-
    cantrip_scale(N).
extend_spell_data('fire bolt', effects, [spell_attack_roll(ranged)]).

