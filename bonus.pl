:- multifile
       bonus/2,
       bonus_source/2,
       bonuses_from_source/2.

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
