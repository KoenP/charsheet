% TODO
base_ability(str, 12).
base_ability(dex, 12).
base_ability(con, 12).
base_ability(wis, 12).
base_ability(cha, 12).
base_ability(int, 18).

name('Barton').
choice(init, 'base race', human).
choice(race(human), subrace, variant).
choice(init, 'initial class', rogue).

choice(initial_class(rogue), skill, [stealth, 'sleight of hand', perception, investigation]).
choice(match_class(rogue:1), expertise, [stealth, investigation]).
