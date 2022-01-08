:- doc_server(4000).
:- set_prolog_flag(toplevel_print_anon, false).
:- portray_text(true).

:- [main/main].

% TODO
base_ability(str, 12).
base_ability(dex, 12).
base_ability(con, 12).
base_ability(wis, 12).
base_ability(cha, 12).
base_ability(int, 18).

name('mr fighter').
choice(init, 'base race', human).
choice(race(human), subrace, variant).
%choice(race(human(variant)), skill, history).
%choice(race(human(variant)), asi, [int+1, con+1]).

choice(init, 'initial class', warlock).

choice(match_class(warlock:1), cantrip, ['blade ward', 'eldritch blast']).
