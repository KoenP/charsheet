:- multifile
    subrace_option/2,
    racial_speed/3,
    race_shorthand/2,
    race_option/1,
    grant_racial_asis_plus2_plus1/1.

:- [race/dragonborn].
:- [race/dwarf].
:- [race/elf].
:- [race/gnome].
:- [race/half_elf].
:- [race/half_orc].
:- [race/halfling].
:- [race/human].
:- [race/tiefling].

:- [race/bugbear].
:- [race/tabaxi].
:- [race/firbolg].

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

%! grant_racial_asis_plus2_plus1(?Race)
%
%  For many races the player gets to pick one ability to increase by 2,
%  and a different ability to increase by 1.
grant_racial_asis_plus2_plus1(_Race) :- false.

bonus_options_source(race(Race), 'ability + 2', id, ability_plus_n(2)) :-
    grant_racial_asis_plus2_plus1(Race).
bonus_options_source(race(Race), 'ability + 1', id, ability_plus_n(1)) :-
    grant_racial_asis_plus2_plus1(Race).
hide_base_option(race(Race), 'ability + 1', Abi+1) :-
    bonus(choice(race(Race), 'ability + 2'), Abi+2),
    grant_racial_asis_plus2_plus1(Race).
hide_base_option(race(Race), 'ability + 2', Abi+2) :-
    bonus(choice(race(Race), 'ability + 1'), Abi+1),
    grant_racial_asis_plus2_plus1(Race).
problem(cant_stack_racial_asis(Race, Abi)) :-
    choice(race(Race), 'ability + 2', Abi + 2),
    choice(race(Race), 'ability + 1', Abi + 1).
