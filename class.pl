:- multifile
       choose_subclass_level/2,
       spell_available/1,
       spell_known/1,
       class_option/1,
       class_trait/2,
       class_trait_options/3,
       subclass_trait/3,
       subclass_trait_options/4,
       hd_per_level/2,
       max_hp_per_level/2,
       saving_throw/2,
       gain_spell_slots/3.

:- [classes/druid].

% Compute class level.
class(Class) :-
    class_level(Class:_).

class_level(Class:ClassLevel) :-
    level(CharLevel),
    class_option(Class),
    findall(L, (initial_class(Class) ; (between(2,CharLevel,L), gain_level(L,Class,_))), Levels),
    length(Levels, ClassLevel),
    ClassLevel > 0.

matching_class_level(Class:ClassLevel) :-
    class_level(Class:CharClassLevel),
    between(1, CharClassLevel, ClassLevel).

% Query the PC's current (sub)class(es).
subclass(Class, Subclass) :-
    chosen_trait(class(Class:_), subclass, subclass(Class, Subclass)).

% choose_subclass/2: shorthand for selecting subclass.
choose_traits(class(Class:Level), subclass, [subclass(Class, Subclass)]) :-
    choose_subclass_level(Class, Level),
    choose_subclass(Class, Subclass).

% Generate a class trait option for each subclass.
class_trait_options(Class:Level, subclass, 1 from Subclasses) :-
    choose_subclass_level(Class, Level),
    findall(subclass(Class, Subclass), subclass_option(Class, Subclass), Subclasses).
    

% Select the right class traits.
trait(class(ClassLevel), Trait) :-
    matching_class_level(ClassLevel),
    class_trait(ClassLevel, Trait).

trait_options(class(Class), Name, Spec) :-
    class(Class),
    class_trait_options(Class, Name, Spec).
trait_options(class(ClassLevel), Name, Spec) :-
    matching_class_level(ClassLevel),
    class_trait_options(ClassLevel, Name, Spec).

trait(subclass(Class:Level, Subclass), Trait) :-
    matching_class_level(Class:Level),
    subclass_trait(Class:Level, Subclass, Trait),
    subclass(Class, Subclass).
trait_options(subclass(Class:Level, Subclass), Name, Spec) :-
    matching_class_level(Class:Level),
    subclass_trait_options(Class:Level, Subclass, Name, Spec),
    subclass(Class, Subclass).
    

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Some class-generic spellcasting stuffs.

% Query your class spells.
class_cantrip(Class, Cantrip) :- 
    spell(Cantrip, level, 0),
    spell(Cantrip, class, Class).

% PC knows all cantrips that you explicitly selected.
spell_known(Class, Cantrip) :-
    chosen_trait(class(Class:_), cantrip, Cantrip).
