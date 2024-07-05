%:- table bonus/2 as incremental.

%! bonus(?Source, ?Bonus)
%
%  Bonuses are properties of your character that typically stack.
%  For example, if your character is an elf,
%  bonus(race(elf), dex+2) would be true.
%  This bonus would stack with other improvements to your dexterity.
%  For most properties that don't stack, such as proficiencies or
%  class features, see trait/2.
%
%  List of valid Bonus terms (should try to keep this exhaustive):
%  - `Abi+N`, where ability(Abi) and integer(N). For example `int+1`,
%    `str+2`, ...
%  - `modify_spell(Origin, Name, Mod)` where
%    `known_spell(Origin,Name,_,_,_,_)`.
%    `Mod` is a binary goal such that `call(Mod,
%    OldData, NewData)` produces the new spell data from the old spell
%    data.
bonus(Source, Bonus) :-
    bonus_source(Source, Bonus),
    call(Source).
bonus(choice(Source, Id), Bonus) :-
    choice_member(Source, Id, Choice),
    choice_member_to_bonus(Source, Id, Goal),
    call(Goal, Choice, Bonus).

%! bonus(Bonus)
%
%  Shorthand for bonus/2, when you're not interested in the source.
bonus(Bonus) :- bonus(_, Bonus).

%! bonus_source(?Source, ?Bonus)
%
%  Each bonus_source/2 clause gives rise to a corresponding
%  bonus/2 clause, *if* call(Source) is true.
bonus_source(Source, Bonus) :-
    bonuses_from_source(Source, Bonuses),
    member(Bonus, Bonuses).

%! bonuses_from_source(?Source, ?Bonuses)
%
%  Equivalent to asserting a bonus_source(Source, Bonus) clause for
%  each member(Bonus, Bonuses).
bonuses_from_source(_,_) :- false.

%! bonus_options_source(?Source, ?Id, ?ToBonus, ?Spec)
%  
%  Each bonus_options_source/4 clause gives rise to a corresponding
%  bonus_options/4 clause, *if* call(Source) is true.
bonus_options_source(_,_,_,_) :- false.

%! bonus_options(?Source, ?Id, ?Spec, ?ToBonus)
%
%  Each clause gives rise to a corresponding options/3 clause,
%  as well as a corresponding choice_member_to_bonus/3 clause.
bonus_options(Source, Id, ToBonus, Spec) :-
    bonus_options_source(Source, Id, ToBonus, Spec),
    call(Source).
options(Source, Id, Spec) :-
    bonus_options(Source, Id, _, Spec).

%! sum_bonuses(++Stat, ?Total:int)
%
%  Sum up all the bonuses that affect Stat.
sum_bonuses(Stat, Total) :-
    ground(Stat),
    findall(Bon, bonus(Stat + Bon), Bonuses),
    sumlist(Bonuses, Total).

%! choice_member_to_bonus(Source, Id, ToBonus)
%
%  Every clause of this predicate declares that some choice/3 clause
%  should give rise to (a) corresponding bonus(es).
%  If choice_member(Source, Id, Choice) is true, then this predicate
%  will make sure bonus(choice(Source, Id), X) is true if call(ToBonus,
%  Choice, X) is true.
choice_member_to_bonus(Source, Id, ToBonus) :-
    bonus_options(Source, Id, ToBonus, _).

id(X,X).

%! bonus_from_level_reached(Level:int, ?Origin, ?Trait)
%
%  True iff Bonus was gained at the given Level automatically upon
%  reaching that level (that is, at that level no active choice was
%  required).
bonus_from_level_reached(Level, Origin, Bonus) :-
    bonus(Origin, Bonus),
    Origin \= choice(_,_),
    origin_level(Origin, Level).
