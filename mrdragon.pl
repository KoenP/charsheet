:- doc_server(4000).
:- set_prolog_flag(toplevel_print_anon, false).
:- portray_text(true).

:- [main/main].

base_ability(str, 8).
base_ability(dex, 13).
base_ability(con, 14).
base_ability(wis, 12).
base_ability(cha, 15).
base_ability(int, 10).

name("Mr Dragon").

% Race.
choice(init, 'base race', human).
choice(race(human), subrace, standard).

% Class.
choice(init, 'initial class', warlock).
choice(match_class(warlock:1), subclass, fiend).
choice(match_class(warlock:1), cantrip, ['mage hand', 'eldritch blast']).
choice(match_class(warlock), spell, ['burning hands', 'arms of hadar']).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
gain_level(2, warlock, hp_avg).
choice(match_class(warlock:2), 'eldritch invocation',
       ['agonizing blast', 'armor of shadows']).
