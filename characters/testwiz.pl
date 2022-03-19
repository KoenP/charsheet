base_ability(str, 12).
base_ability(dex, 12).
base_ability(con, 12).
base_ability(wis, 12).
base_ability(cha, 12).
base_ability(int, 18).
name("Testwiz").

choice(init, 'base race', human).
choice(race(human), subrace, variant).
choice(init, 'initial class', wizard).

choice(class(wizard), cantrip, ['fire bolt', message, mending]).
