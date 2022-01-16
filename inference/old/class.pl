:- multifile
       % Register whether a class is a full or partial caster,
       % e.g. caster(wizard, full), caster(ranger, 1/2), ...
       % For purposes of calculating spell slots.
       caster/2,

       spellcasting_ability/2,
       spellcasting_availability/2,

       max_prepared_spells/2,
       choose_subclass_level/2,
       spell_available/1,

       class_option/1,
       subclass_option/2,

       class_trait/2,
       class_trait_options/3,
       class_trait_replacement_options/4,
       wrap_class_trait_option/4,

       subclass_trait/3,
       subclass_trait_options/4,
       wrap_subclass_trait_option/5,

       hd_per_level/2,
       initial_class_base_hp/2,
       max_hp_per_level/2,
       class_saving_throw/2,
       gain_spell_slots/3.

:- [classes/druid].
:- [classes/sorcerer].
:- [classes/fighter].
:- [classes/warlock].
:- [classes/wizard].
:- [classes/ranger].
   
:- table
   class_level/1,
   class_level/2.

% Compute class level.
class(Class) :-
    class_level(Class:_).

class_level(Class:ClassLevel) :-
    level(CharLevel),
    class_level(CharLevel, Class:ClassLevel).
:- table class_level/2.
class_level(CharLevel, Class:ClassLevel) :-
    class_option(Class),
    findall(L, (initial_class(Class)
               ; (between(2,CharLevel,L), gain_level(L,Class,_))),
            Levels),
    length(Levels, ClassLevel),
    ClassLevel > 0.

matching_class_level(Class:ClassLevel) :-
    class_level(Class:CharClassLevel),
    between(1, CharClassLevel, ClassLevel).

matching_class_or_class_level(Class) :-
    class(Class).
matching_class_or_class_level(Class:Level) :-
    matching_class_level(Class:Level).

class_levels(Classes) :- findall(Class, class_level(Class), Classes).

:- table reached_class_level_on_char_level/2.
reached_class_level_on_char_level(Class:ClassLevel, CharLevel) :-
    level(CurLevel),
    findall(L,
            ((initial_class(Class), L=1) ; (between(2,CurLevel,L), gain_level(L,Class,_))),
            Ls),
    length(Prefix, ClassLevel),
    append(Prefix, _, Ls),
    last(Prefix, CharLevel).

multiclass :-
    findall(Class, class(Class), [_,_|_]).

% Calculate a class' contribution to multiclass spell slot level.
class_spell_slot_level(Class, SpellSlotLevel) :-
    class_level(Class:Level),
    caster(Class, Factor),
    spell_slot_factor(Factor, Level, SpellSlotLevel).
spell_slot_factor(full, Level, Level).
spell_slot_factor(1 / N, Level, SpellSlotLevel) :-
    SpellSlotLevel is floor(Level / N).

% Query the PC's current (sub)class(es).
subclass(Class, Subclass) :-
    chosen_trait(class(Class:_), subclass, subclass(Class, Subclass)).

% choose_subclass/2: shorthand for selecting subclass.
choose_traits(class(Class:Level), subclass, [subclass(Class, Subclass)]) :-
    choose_subclass_level(Class, Level),
    choose_subclass(Class, Subclass).

% Query traits that originate from your class, directly or indirectly.
trait_from_class(Class, Origin, Trait) :-
    trait(Origin, Trait),
    class_origin(Class, Origin), !.
trait_from_class_level(Class:Level, Origin, Trait) :-
    trait(Origin, Trait),
    class_level_origin(Class:Level, Origin).

class_origin(Class, class(Class:_)).
class_origin(Class, class(Class)).
class_origin(Class, subclass(Class:_, _)).
class_origin(Class, subclass(Class, _)).
class_origin(Class, choose_traits(class(Class:_), _)).
class_origin(Class, choose_traits(class(Class), _)).
class_origin(Class, choose_traits(subclass(Class:_, _), _)).
class_origin(Class, choose_traits(subclass(Class, _), _)).

class_level_origin(Class:Level, class(Class:Level)).
class_level_origin(Class:Level, subclass(Class:Level, _)).
class_level_origin(Class:Level, choose_traits(class(Class:Level), _)).
class_level_origin(Class:Level, choose_traits(subclass(Class:Level, _), _)).

% Generate a class trait option for each subclass.
class_trait_options(Class:Level, subclass, 1 from Subclasses) :-
    choose_subclass_level(Class, Level),
    findall(subclass(Class, Subclass), subclass_option(Class, Subclass), Subclasses).

% Select the right class traits.
gain_trait(Level, class(ClassLevel), Trait) :-
    matching_class_level(ClassLevel),
    class_trait(ClassLevel, Trait),
    reached_class_level_on_char_level(ClassLevel, Level).

% Class trait options.
trait_options(class(C), Name, Spec) :-
    matching_class_or_class_level(C),
    class_trait_options(C, Name, Spec).
trait_replacement_options(class(C), Name, OldSpec, NewSpec) :-
    matching_class_or_class_level(C),
    class_trait_replacement_options(C, Name, OldSpec, NewSpec).

gain_trait(CharLevel, subclass(Class:Level, Subclass), Trait) :-
    matching_class_level(Class:Level),
    subclass_trait(Class:Level, Subclass, Trait),
    subclass(Class, Subclass),
    reached_class_level_on_char_level(Class:Level, CharLevel).
trait_options(subclass(Class:Level, Subclass), Name, Spec) :-
    matching_class_level(Class:Level),
    subclass_trait_options(Class:Level, Subclass, Name, Spec),
    subclass(Class, Subclass).

wrap_trait_option(class(C), Name, X, Y) :-
    wrap_class_trait_option(C, Name, X, Y).
wrap_trait_option(subclass(Class, Subclass), Name, X, Y) :-
    wrap_subclass_trait_option(Class, Subclass, Name, X, Y).

wrap_trait_replacement_option(class(Class), Name, A, B, C, D) :-
    wrap_class_trait_replacement_option(Class, Name, A, B, C, D).
wrap_trait_replacement_option(subclass(Class, Subclass), Name, A, B, C, D) :-
    wrap_subclass_trait_replacement_option(Class, Subclass, Name, A, B, C, D).
    
% PC is proficient in the saving throws of their initial class.
saving_throw_prof(Ability) :-
    initial_class(Class),
    class_saving_throw(Class, Ability).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Some class-generic spellcasting stuff.

% Query your class spells.
class_cantrip(Class, Cantrip) :- 
    spell(Cantrip, level, 0),
    spell(Cantrip, class, Class).

list_class_cantrips(Class, Cantrips) :-
    findall(Cantrip, class_cantrip(Class, Cantrip), Cantrips).