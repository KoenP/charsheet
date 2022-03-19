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

choice(race(human), language, elvish).
choice(race(human(variant)), skill, arcana).
choice(initial_class(rogue), skill, [stealth, 'sleight of hand', perception, investigation]).
choice(match_class(rogue:1), expertise, [stealth, investigation]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
gain_level(2, rogue, hp_avg).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
gain_level(3, rogue, hp_avg).
choice(match_class(rogue:3), subclass, 'arcane trickster').
choice(match_class(rogue('arcane trickster'):3), cantrip,
       ['booming blade', 'minor illusion']).
choice(match_class(rogue('arcane trickster'):3), 'illusion or enchantment',
       ['silent image', 'disguise self']).
choice(match_class(rogue('arcane trickster'):3), 'unconstrained spell',
       'find familiar').

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
gain_level(4, rogue, hp_avg).
choice(match_class(rogue('arcane trickster'):4), 'illusion or enchantment',
       'tasha\'s hideous laughter').

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
gain_level(5, rogue, hp_avg).
