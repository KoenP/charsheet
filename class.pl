:- multifile
       % Register whether a class is a full or partial caster,
       % e.g. caster(wizard, full), caster(ranger, 1/2), ...
       % For purposes of calculating spell slots.
       caster/2,

       max_prepared_spells/2,
       spell_class/2,
       choose_subclass_level/2,
       spell_available/1,

       class_option/1,
       subclass_option/2,

       class_trait/2,
       class_trait_options/3,

       subclass_trait/3,
       subclass_trait_options/4,


       hd_per_level/2,
       initial_class_base_hp/2,
       max_hp_per_level/2,
       class_saving_throw/2,
       gain_spell_slots/3.

:- [classes/druid].
:- [classes/fighter].
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

class_levels(Classes) :- findall(Class, class_level(Class), Classes).

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
    
% PC is proficient in the saving throws of their initial class.
saving_throw_prof(Ability) :-
    initial_class(Class),
    class_saving_throw(Class, Ability).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Some class-generic spellcasting stuff.

% Query your class spells.
class_cantrip(Class, Cantrip) :- 
    spell(Cantrip, level, 0),
    spell_class(Cantrip, Class).

% PC knows all cantrips that you explicitly selected.
spell_known(Spell, Class, Ability, always_available, at_will) :-
    class(Class),
    trait(learn_spell(Class, Spell)),
    spellcasting_ability(Class, Ability),
    spell(Spell, level, 0).
