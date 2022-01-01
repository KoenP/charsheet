:- multifile
       bonus/2,
       bonus_source/2,
       bonuses_from_source/2,
       bonus_options_source/4.

%! bonus(?Source, ?Bonus)
%
%  Bonuses are properties of your character that stack.
%  For example, if your character is an elf,
%  bonus(race(elf), dex+2) would be true.
%  This bonus would stack with other improvements to your dexterity.
%  For most properties that don't stack, such as proficiencies or
%  class features, see trait/2.
bonus(Source, Bonus) :-
    bonus_source(Source, Bonus),
    call(Source).

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
