:- multifile
       class_option/1,
       subclass_options/2,
       hd_per_level/2,
       initial_class_base_hp/2,
       max_hp_per_level/2,
       class_saving_throw/2.

:- discontiguous
       required_predicate_for_each_class/1.

:- [class/sorcerer].

% Druid example (to move)
class_option(druid).
subclass_options(druid:2, [land, moon]).

%! class_level(?ClassLevel)
%
%  class_level(Class:Level) is true iff your character has reached
%  exactly level Level in class Class, as determined by the clauses of
%  gain_level/3.
class_level(Class:ClassLevel) :-
    class_option(Class), % ground
    findall(L, (initial_class(_) ; gain_level(L, Class, _)), Levels),
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
    subclass_options(Class:_, _),
    choice(match_class(Class:_), subclass, Sub),
    Subclass =.. [Class, Sub].
subclass_level(Subclass:Level) :-
    subclass(Subclass),
    Subclass =.. [Class, _],
    class_level(Class:Level).

%! subclass_options(ClassLevel, OptionList)
%
%  For each class with subclasses to choose from (which is normally
%  all of them, but the program doesn't require a class to have
%  subclasses), we have a clause subclass_options(Class:Level,
%  OptionList) which shows the level requirement to pick your
%  character's subclass, as well as the options to pick from.
subclass_options(_,_) :- false.
options(match_class(Class:ClassLevel), subclass, from_list(SubclassOptions)) :-
    subclass_options(Class:ClassLevel, SubclassOptions).

%! match_class(?X)
%
%  Deterimine whether your character matches a class/subclass (level) requirement.
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
required_predicate_for_each_class(inital_class_base_hp/2).

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
