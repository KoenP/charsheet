:- multifile
    subrace/2,
    race_option/1.

:- [race/elf].
:- [race/human].

race(Race) :-
    choice(_, 'base race', Race).
race(Race) :-
    choice(race(BaseRace), subrace, Subrace),
    Race =.. [BaseRace, Subrace].

options(init, 'base race', race_option).
options(race(Race), subrace, subrace(Race)) :-
    race(Race),
    subrace(Race, _).
