:- multifile
       trait/1,
       trait/2,
       gain_trait/3,
       lose_trait/3.

table gain_trait/3, lose_trait/3.

:- table trait/1, trait/2.
trait(Trait) :- trait(_, Trait).
trait(Origin, Trait) :-
    level(Level),
    trait_at_level(Level, Origin, Trait).

% Traits per level, including "indirect" traits (traits received from)
% other traits.
:- table trait_at_level/3.
trait_at_level(Level, Origin, Trait) :-
    root_trait_at_level(Level, Origin, Trait).
trait_at_level(Level, Trait, Effect) :-
    trait_effect(Trait, Effect),
    trait_at_level(Level, _, Trait).

trait_at_class_level(Class:Level, Origin, Trait) :-
    matching_class_level(Class:Level), % to ground
    reached_class_level_on_char_level(Class:Level, CharLevel),
    trait_at_level(CharLevel, Origin, Trait).

% Root traits are those directly gained, not as an effect from another trait.
:- table root_trait_at_level/3.
root_trait_at_level(Level, Origin, Trait) :-
    gain_trait(Level, Origin, Trait).
root_trait_at_level(Level, Origin, Trait) :-
    level(CurLevel),
    between(2, CurLevel, Level),
    PrevLevel is Level - 1,
    root_trait_at_level(PrevLevel, Origin, Trait),
    \+ lose_trait(Level, _, Trait).

% From each trait origin, we can derive the character level on which
% it was reached.
trait_origin_level(class(ClassLevel), CharLevel) :-
    reached_class_level_on_char_level(ClassLevel, CharLevel).
trait_origin_level(class(Class), CharLevel) :-
    Class \= _:_,
    reached_class_level_on_char_level(Class:1, CharLevel).
trait_origin_level(subclass(ClassLevel,_), CharLevel) :-
    reached_class_level_on_char_level(ClassLevel, CharLevel).
trait_origin_level(subclass(Class,_), CharLevel) :-
    Class \= _:_,
    reached_class_level_on_char_level(Class:1, CharLevel).
trait_origin_level(background(_), 1).
