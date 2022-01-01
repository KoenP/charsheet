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

name('Tristan Harpell').
choice(init, 'base race', human).
choice(race(human), subrace, variant).
choice(race(human(variant)), skill, history).
choice(race(human(variant)), asi, [int+1, con+1]).

% Level 1 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

choice(init, 'initial class', wizard).
choice(initial_class(wizard), skill, [arcana, investigation]).
choice(class(wizard), cantrip, ['dancing lights', frostbite, 'minor illusion']).
% poison spray, ray of frost, mold earth, fire bolt
choice(class(wizard), 'free spell',
       ['detect magic', 'unseen servant', thunderwave, shield, 'magic missile', 'ice knife']).

% Level 2 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

gain_level(2, wizard, hp_avg).
choice(match_class(wizard:2), subclass, evocation).
choice(match_class(wizard:2), 'free spell', ['burning hands', 'comprehend languages']).

% Level 3 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

gain_level(3, wizard, hp_avg).
choice(match_class(wizard:3), 'free spell', [shatter, 'see invisibility']).
% misty step, mirror image, magic weapon, darkvision, aganazzar's scorcher
% scrolls: levitate? knock? invisibility? detect thoughts? rope trick?

% Level 4 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

gain_level(4, wizard, hp_avg).
choice(match_class(wizard:4), cantrip, 'fire bolt').
choice(match_class(wizard:4), 'free spell', [darkvision, 'misty step']).

% Level 5 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

gain_level(5, wizard, hp_avg).
choice(match_class(wizard:5), 'free spell', [counterspell, fireball]).
