:- multifile
       racial_trait/2.

:- [races/tortle].

trait(race(Race), Trait) :-
    race(Race),
    racial_trait(Race, Trait).

racial_abi(Ability+Increment) :-
    trait(race(_), Ability+Increment).
