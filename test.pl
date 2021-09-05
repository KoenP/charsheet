:- doc_server(4000).
:- portray_text(true).


:- [sketch].


% Level 1
race(elf).
race('high elf').
choice(init, 'initial class', sorcerer).
choice(initial_class(sorcerer), skill, [arcana,deception]).
choice(class(sorcerer), spell, [thunderwave, blur]).
choice(class(sorcerer), cantrip, ['acid splash', 'blade ward', 'booming blade', 'chill touch']).

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
choice(match_class(sorcerer:4), cantrip, 'true strike').
