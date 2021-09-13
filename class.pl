:- multifile
       class_option/1,
       choose_subclass_level/1,
       subclass_option/2,
       hd_per_level/2,
       initial_class_base_hp/2,
       max_hp_per_level/2,
       class_saving_throw/2,
       caster/2.

:- discontiguous
       required_predicate_for_each_class/1.

:- [class/sorcerer].

%! class_level(?ClassLevel)
%
%  class_level(Class:Level) is true iff your character has reached
%  exactly level Level in class Class, as determined by the clauses of
%  gain_level/3.
class_level(Class:ClassLevel) :-
    class_option(Class), % ground
    findall(L, (initial_class(Class) ; gain_level(L, Class, _)), Levels),
    length(Levels, ClassLevel),
    ClassLevel > 0.

%! class(?Class)
%
%  True for each Class your character has at least one level in.
class(Class) :- class_level(Class:_).

%! subclass(?Subclass)
%
%  Subclass is a compound term, where the class name is the functor
%  and the subclass atom is the sole argument. True iff your character
%  has at least one level in that class, and has selected the
%  subclass option with a choice/3 clause.
subclass(Subclass) :-
    class(Class),
    choice(match_class(Class:_), subclass, Sub),
    Subclass =.. [Class, Sub].
subclass_level(Subclass:Level) :-
    subclass(Subclass),
    Subclass =.. [Class, _],
    class_level(Class:Level).

%! subclass_option(?Class, ?Subclass)
subclass_option(_,_) :- false.
options_source(match_class(Class:ClassLevel), subclass, subclass_option(Class)) :-
    choose_subclass_level(Class:ClassLevel).

%! match_class(?X)
%
%  Determine whether your character matches a class/subclass (level) requirement.
%  If, for example, your character has class_level(warlock:3) with the
%  `fiend` subclass and class_level(fighter:1), then match_class(X) is
%  true for X equal to any of:
%
%  * `warlock:3`
%  * `warlock:2`
%  * `warlock:1`
%  * `warlock(fiend):3`
%  * `warlock(fiend):2`
%  * `warlock(fiend):1`
%  * `warlock`
%  * `warlock(fiend)`
%  * `fighter:1`
%  * `fighter`
match_class(C:L1) :-
    (class_level(C:L2) ; subclass_level(C:L2)),
    between(1, L2, L1).
match_class(C) :-
    class(C) ; subclass(C).

%! class_origin_to_class_level(?Origin, ?Level:int)
%
%  Given a class-related Origin (for a trait, or a choice, or ...),
%  determine what class level that Origin refers to.
class_origin_to_class_level(choice(Origin,_), ClassLevel) :-
    class_origin_to_class_level_(Origin, ClassLevel).
class_origin_to_class_level(Origin, ClassLevel) :-
    class_origin_to_class_level_(Origin, ClassLevel).
class_origin_to_class_level_(class(Class), Class:1).
class_origin_to_class_level_(subclass(Subclass), Class:Lvl) :-
    Subclass =.. [Class, _],
    choose_subclass_level(Class:Lvl).
class_origin_to_class_level_(initial_class(Class), Class:1).
class_origin_to_class_level_(match_class(ClassF:Level), Class:Level) :-
    ClassF =.. [Class|_].
%class_origin_to_class_level_(replaced_spell(Class:Level, _), Class:Level).

%! class_origin_to_class(?Origin, ?Class:atomic)
%
%  Given a class-related Origin (for a trait, or a choice, or ...),
%  determine what class that Origin refers to.
class_origin_to_class(Origin, Class) :-
    class_origin_to_class_level(Origin, Class:_).


%! multiclass
%
%  True iff your character has more than one class.
multiclass :-
    findall(C, class(C), [_,_|_]).

%! hd_per_level(?Class, ?Dice)
%
%  Hit dice gained per level in the given Class.
%  This predicate must be defined for each class.
hd_per_level(_,_) :- false.
required_predicate_for_each_class(hd_per_level/2).

%! initial_class_base_hp(?Class, ?HP)
%
%  The base_hp/1 of a character in Class of character level 1.
%  This predicate must be defined for each class.
initial_class_base_hp(_,_) :- false.
required_predicate_for_each_class(initial_class_base_hp/2).

%! max_hp_per_level(?Class, ?Dice)
%
%  Dice roll to determine the number of hit points your character
%  gains upon leveling up in the given Class.
%  This predicate must be defined for each class.
max_hp_per_level(_,_) :- false.
required_predicate_for_each_class(max_hp_per_level/2).

%! class_saving_throw(?Class, ?Ability)
%
%  Picking Class as *initial class* makes your character proficient in
%  saving throws for Ability.
class_saving_throw(_,_) :- false.
required_predicate_for_each_class(class_saving_throw/2).

%! choose_subclass_level(?ClassLevel)
%
%  Determines at which ClassLevel for any given class you have to pick
%  your subclass.
choose_subclass_level(_) :- false.

%! caster(?Class, ?Factor)
%
%  Indicates to what extent the given Class is a classical spell caster.
%  From PHB, wizards, bards, sorcerers, druids, and clerics are all full casters,
%  so the query caster(wizard, full) is true.
%  Rangers and paladins are half casters; we write caster(ranger, 1/2).
%  The eldritch knight subclass of the fighter class is a "one third" caster,
%  written as `caster(fighter, 1/3) :- subclass(fighter('eldritch knight'))`
%  Other classes aren't casters, but we assert this explicitly with
%  caster(Class, 0), rather than just omitting the clause altogether.
%  Note that the warlock class is indeed not a caster: warlocks get
%  the `'pact magic'` feature rather than the `spellcasting` feature,
%  which works differently.
caster(_,_) :- false.
required_predicate_for_each_class(caster/2).

%! caster
%
%  True iff your character has at least one level in a class with the
%  spellcasting feature.
caster :-
    class(Class),
    caster(Class, Factor),
    Factor \= 0,
    !.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% meta todo's
meta_todo(class(Class), predicate_missing(Pred/Arity)) :-
    class_option(Class),
    required_predicate_for_each_class(Pred/Arity),
    missing_for_class(Class, Pred/Arity).

missing_for_class(Class, Pred/Arity) :-
    length(Args, Arity),
    Args = [Class|_],
    Goal =.. [Pred|Args],
    \+ call(Goal).
