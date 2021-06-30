:- multifile
       racial_trait/2.

:- [races/tortle].

trait(race(Race), Trait) :-
    race(Race),
    racial_trait(Race, Trait).
trait(race(Race,1), Trait) :-
    trait(race(Race), Trait).

racial_abi(abi(Ability, Increment)) :-
    trait(race(_), abi(Ability, Increment)).
