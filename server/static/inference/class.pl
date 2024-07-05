:- [class/barbarian].
:- [class/bard].
:- [class/cleric].
:- [class/druid].
:- [class/fighter].
:- [class/monk].
:- [class/paladin].
:- [class/ranger].
:- [class/rogue].
:- [class/sorcerer].
:- [class/warlock].
:- [class/wizard].

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
    choice(Class >: _, subclass, Sub),
    Subclass =.. [Class, Sub].
subclass_level(Subclass:Level) :-
    subclass(Subclass),
    Subclass =.. [Class, _],
    class_level(Class:Level).

%! base_class(+Class, -BaseClass)
%
%  Extract the base class from a class.
base_class(Class, BaseClass) :-
    Class =.. [BaseClass|_],
    class_option(BaseClass).

%! subclass_option(?Class, ?Subclass)
subclass_option(_,_) :- false.
options_source(Class >: ClassLevel, subclass, subclass_option(Class)) :-
    choose_subclass_level(Class:ClassLevel).

%! match_class(?X)
%
%  TODO: delete
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

%! >:(?Class, ?Level)
%
%  Shorthand notation for `match_class(Class:Level)`.
Class >: Level :- match_class(Class:Level).

%! ^(?Class)
%
%  Shorthand notation for `initial_class(Class)`.
^ Class :- initial_class(Class).

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
class_origin_to_class_level_(multiclass_into(Class), Class:1).
class_origin_to_class_level_(^Class, Class:1).
class_origin_to_class_level_(ClassF >: Level, X) :-
    class_origin_to_class_level_(match_class(ClassF:Level), X).
class_origin_to_class_level_(match_class(ClassF:Level), Class:Level) :-
    (Tail = [] ; Tail = [_]),
    ClassF =.. [Class|Tail].
class_origin_to_class_level_(match_class(ClassF), Class:1) :-
    (Tail = [] ; Tail = [_]),
    ClassF =.. [Class|Tail].
%class_origin_to_class_level_(replaced_spell(Class:Level, _), Class:Level).

%! gained_level_in_class_at_charlevel(?Class, ?CharLevel)
gained_level_in_class_at_charlevel(Class, 1) :-
    initial_class(Class).
gained_level_in_class_at_charlevel(Class, CharLevel) :-
    gain_level(CharLevel, Class, _).

%! reached_classlevel_at_charlevel(?ClassLevel, ?CharLevel)
reached_classlevel_at_charlevel(Class:ClassLevel, CharLevel) :-
    base_class(Class, BaseClass),
    class_option(BaseClass),
    findall(L, gained_level_in_class_at_charlevel(BaseClass,L), Ls),
    enumerate(1, Ls, NLs),
    member(ClassLevel-CharLevel, NLs).

%! class_origin_to_class(?Origin, ?Class:atomic)
%
%  Given a class-related Origin (for a trait, or a choice, or ...),
%  determine what class that Origin refers to.
class_origin_to_class(Origin, Class) :-
    class_origin_to_class_level(Origin, Class:_).

%! find_origin_class(?Origin, ?Class)
%
%  True if Origin is somehow related to Class.
find_origin_class(Origin, Class) :-
    class_origin_to_class(Origin, Class).
find_origin_class(trait(Trait), Class) :-
    trait(TraitOrigin, Trait),
    find_origin_class(TraitOrigin, Class).

%! class_choice(?Class:atomic, ?Id, ?Choice)
%
%  A choice originating from a class-based option.
class_choice(Class, Id, Choice) :-
    choice_member(Origin, Id, Choice),
    class_origin_to_class(Origin, Class).

%! multiclass
%
%  True iff your character has more than one class.
multiclass :-
    findall(C, class(C), [_,_|_]).

%! multiclass_into(Class)
%
%  True iff your character has Class as one of its classes, but not as
%  its initial_class/1.
multiclass_into(Class) :-
    class(Class),
    \+ initial_class(Class).

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
trait_source(Class >: 1, saving_throw(Abi)) :-
    ^Class,
    class_saving_throw(Class, Abi).

%! choose_subclass_level(?ClassLevel)
%
%  Determines at which ClassLevel for any given class you have to pick
%  your subclass.
choose_subclass_level(_) :- false.

%! asi_level(?ClassLevel)
%
%  Determines at which ClassLevels your character receives an ability
%  score increase.
asi_level(_) :- false.
required_predicate_for_each_class(asi_level/1).
default_asi_level(L) :-
    member(L, [4,8,12,16,19]).

options_source(Class >: Level, 'asi or feat', (2 from ability) or feat_option) :-
    asi_level(Class:Level).
trait(choice(AsiLevel,'asi or feat'), feat(Feat)) :-
    choice(AsiLevel, 'asi or feat', Feat),
    feat_option(Feat).
bonus(choice(AsiLevel,'asi or feat'), Ability+1) :-
    choice_member(AsiLevel, 'asi or feat', Ability),
    ability(Ability).

    %(Bonus = Ability + N ; member(Ability+N, Bonus)).

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
%required_predicate_for_each_class(caster/2).

%! spellcasting_ability(?Class:atomic, ?Ability:atomic)
spellcasting_ability(Compound, Abi) :-
    Compound =.. [Class,_|_],
    spellcasting_ability(Class, Abi).

%! max_prepared_spells(?Class:atomic, ?N:int)
%
%  Number of spells you can prepare for Class. A class that doesn't
%  need to prepare spells (like sorcerer) should just not list a
%  clause for this predicate.
max_prepared_spells(_,_) :- false.

%! caster
%
%  True iff your character has at least one level in a class with the
%  spellcasting feature.
caster :-
    class(Class),
    caster(Class, Factor),
    Factor \= 0,
    !.

%! class_shorthand(?Class:atomic, ?Shorthand:atomic)
%
%  Associates each class with a two-letter shorthand.
class_shorthand(barbarian , bb).
class_shorthand(bard      , bd).
class_shorthand(cleric    , cl).
class_shorthand(druid     , dr).
class_shorthand(fighter   , fi).
class_shorthand(monk      , mo).
class_shorthand(paladin   , pa).
class_shorthand(ranger    , ra).
class_shorthand(rogue     , ro).
class_shorthand(sorcerer  , so).
class_shorthand(warlock   , wl).
class_shorthand(wizard    , wz).

%! class_skill_list(?Class, ?List)
%
%  List of skills you can typically pick from when you gain skills
%  through this class.
class_skill_list(_,_) :- false.

%! class_skill(?Class, ?Skill)
%
%  One of the skills you can typically pick when you gain skills
%  through this class.
class_skill(Class, Skill) :-
    class_skill_list(Class, List),
    member(Skill, List).
class_skill_wrapped(Class, skill(Skill)) :-
    class_skill(Class, Skill).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% meta todo's
meta_todo(class(Class), predicate_missing(Pred/Arity)) :-
    class_option(Class),
    required_predicate_for_each_class(Pred/Arity),
    missing_for_class(Class, Pred/Arity).

missing_for_class(Class, Pred/1) :-
    \+ call(Pred, Class:_).
missing_for_class(Class, Pred/Arity) :-
    Arity \= 1,
    length(Args, Arity),
    Args = [Class|_],
    Goal =.. [Pred|Args],
    \+ call(Goal).
