:- multifile
    subrace_option/2,
    racial_speed/3,
    race_shorthand/2,
    race_option/1.

:- [race/dragonborn].
:- [race/dwarf].
:- [race/elf].
:- [race/gnome].
:- [race/half_elf].
:- [race/half_orc].
:- [race/halfling].
:- [race/human].
:- [race/tabaxi].
:- [race/tiefling].

race(Race) :-
    choice(_, 'base race', Race).
race(Race) :-
    choice(race(BaseRace), subrace, Subrace),
    Race =.. [BaseRace, Subrace].

most_specific_race(Race) :-
    race(Race),
    \+ subrace_option(Race, _).

race_has_subraces(Race) :-
    subrace_option(Race, _),
    !.

options(init, 'base race', race_option).
options(race(Race), subrace, subrace_option(Race)) :-
    race(Race),
    race_has_subraces(Race).

racial_speed(Race, WalkingSpeed) :- racial_speed(Race, walking, WalkingSpeed).
racial_speed(_,_,_) :- false.
race_shorthand(_,_) :- false.

meta_todo(race(Race), 'missing racial speed') :-
    race_option(Race),
    \+ racial_speed(Race, _).

meta_todo(race(Race), 'missing race shorthand') :-
    race_option(Race),
    \+ race_shorthand(Race, _).
