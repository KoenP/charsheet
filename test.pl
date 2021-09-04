:- doc_server(4000).
:- portray_text(true).


:- [sketch].


% Level 1
race(elf).
race('high elf').
choice(init, 'base class', druid).

% Level 2
gain_level(2, druid, hp_avg).
choice(match_class(druid:2), subclass, moon).

% Level 3
gain_level(3, druid, hp_avg).
