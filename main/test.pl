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
name('Zahar') :- !.
race(elf).
race('high elf').
choice(init, 'initial class', sorcerer).
choice(initial_class(sorcerer), skill, [arcana,deception]).
choice(class(sorcerer), spell, [thunderwave, blur]).
choice(class(sorcerer), cantrip, ['acid splash', 'blade ward', 'booming blade', 'chill touch']).
choice(match_class(sorcerer:1), subclass, 'draconic bloodline').
choice(_, 'dragon ancestor', red).

% Level 2
gain_level(2, sorcerer, hp_avg).
choice(match_class(sorcerer:2), spell, 'false life').
choice(match_class(sorcerer:2), replace(spell), thunderwave).
choice(match_class(sorcerer:2), replacing(spell, thunderwave), darkvision).

% Level 3
gain_level(3, sorcerer, hp_avg).
choice(match_class(sorcerer:3), spell, 'inflict wounds').
choice(match_class(sorcerer:3), replace(spell), darkvision).
choice(match_class(sorcerer:3), replacing(spell, darkvision), 'scorching ray').

% Level 4
gain_level(4, sorcerer, hp_avg).
choice(match_class(sorcerer:4), cantrip, 'fire bolt').
choice(match_class(sorcerer:4), spell, fireball).

choice(match_class(sorcerer:4), 'asi or feat', feat(alert)).

gain_level(5, sorcerer, hp_avg).
gain_level(6, sorcerer, hp_avg).
%gain_level(7, sorcerer, hp_avg).
%gain_level(8, sorcerer, hp_avg).
%
%gain_level(L, sorcerer, hp_avg) :- between(9, 20, L).