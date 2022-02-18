
base_ability(str, 12).
base_ability(dex, 12).
base_ability(con, 12).
base_ability(wis, 12).
base_ability(cha, 12).
base_ability(int, 12).

name('Jehnlana').
choice(init, 'base race', human).
choice(race(human), subrace, standard).

% Level 1 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

choice(init, 'initial class', gambler).
choice(initial_class(gambler), skill, [athletics, insight, perception]).

gain_level(2, gambler, hp_avg).
gain_level(3, gambler, hp_avg).
gain_level(4, gambler, hp_avg).
gain_level(5, gambler, hp_avg).
