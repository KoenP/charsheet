:- doc_server(4000).
:- set_prolog_flag(toplevel_print_anon, false).
:- portray_text(true).

:- [sketch].

% TODO
base_ability(str, 12).
base_ability(dex, 12).
base_ability(con, 12).
base_ability(wis, 12).
base_ability(cha, 18).
base_ability(int, 12).

% Level 1
name('Mr Warlock') :- !.
race(elf).
race('high elf').
choice(init, 'initial class', warlock).
choice(match_class(warlock:1), subclass, fiend).
choice(match_class(warlock:1), cantrip, ['eldritch blast', 'blade ward']).


gain_level(2, warlock, hp_avg).
choice(match_class(warlock:2), 'eldritch invocation', ['beast speech', 'agonizing blast']).
choice(match_class(warlock:2), spell, 'hold monster').

gain_level(3, warlock, hp_avg).
choice(match_class(warlock:3), replace('eldritch invocation'), 'beast speech').
choice(match_class(warlock:3), replacing('eldritch invocation', 'beast speech'), 'beguiling influence').
choice(match_class(warlock:3), 'pact boon', chain).

gain_level(4, warlock, hp_avg).
choice(match_class(warlock:4), replace('eldritch invocation'), 'beguiling influence').
choice(match_class(warlock:4), replacing('eldritch invocation', 'beguiling influence'),
       'bewitching whispers').

gain_level(5, warlock, hp_avg).
gain_level(6, warlock, hp_avg).
gain_level(7, warlock, hp_avg).
gain_level(8, warlock, hp_avg).
gain_level(9, warlock, hp_avg).
gain_level(10, warlock, hp_avg).

gain_level(11, warlock, hp_avg).
choice(_, 'arcanum spell', 'wall of ice').


trait(_, eldritch_invocation('eldritch spear')).
trait(_, eldritch_invocation('otherworldly leap')).
trait(_, eldritch_invocation('misty visions')).
trait(_, eldritch_invocation('minions of chaos')).
trait(_, eldritch_invocation('repelling blast')).
