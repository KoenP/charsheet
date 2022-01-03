:- multifile
    subrace/2,
    racial_speed/2,
    race_shorthand/2,
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

racial_speed(_,_) :- false.
race_shorthand(_,_) :- false.

meta_todo(race(Race), 'missing racial speed') :-
    race_option(Race),
    \+ racial_speed(Race, _).

meta_todo(race(Race), 'missing race shorthand') :-
    race_option(Race),
    \+ race_shorthand(Race, _).
