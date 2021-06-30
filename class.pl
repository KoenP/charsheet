:- multifile
    class_option/1,
    class_trait/2,
    class_trait_options/4,
    hd_per_level/2,
    max_hp_per_level/2,
    saving_throw/2,
    gain_spell_slots/3.

:- [classes/druid].

class_cantrip(Class, Cantrip) :- 
    spell(Cantrip, level, 0),
    spell(Cantrip, class, Class).


trait(class(Class), Trait) :-
    class(Class),
    class_trait(Class, Trait).
trait(class(ClassLevel), Trait) :-
    matching_class_level(ClassLevel),
    class_trait(ClassLevel, Trait).

trait_options(class(Class), Name, Nr, Trait) :-
    class(Class),
    class_trait_options(Class, Name, Nr, Trait).
trait_options(class(ClassLevel), Name, Nr, Options) :-
    matching_class_level(ClassLevel),
    class_trait_options(ClassLevel, Name, Nr, Options).

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

%class(Class) :-
%    initial_class(Class).
%class(Class) :-
%    findall(C, gain_level(_,C,_), Classes),
%    list_to_set(Classes, ClassesSet),
%    member(Class, ClassesSet).
%class(Class, ClassLevel) :-
%    level(CharLevel),
%    class_option(Class),
%    findall(L, (between(2,CharLevel,L),gain_level(L,Class,_)), Levels),
%    (initial_class(Class) -> X = 1 ; X = 0),
%    length(Levels, Len),
%    ClassLevel is Len + X,
%    ClassLevel > 0.
