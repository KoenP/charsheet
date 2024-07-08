name('Barton').
base_ability(str,8).
base_ability(dex,17).
base_ability(con,15).
base_ability(int,8).
base_ability(wis,12).
base_ability(cha,16).
gain_level(2,rogue,hp_avg).
gain_level(3,rogue,hp_avg).
gain_level(4,rogue,hp_avg).
gain_level(5,rogue,hp_avg).
choice(init,'base race',human).
choice(race(human),subrace,variant).
choice(race(human),language,elvish).
choice(race(human(variant)),asi,[dex+1,con+1]).
choice(race(human(variant)),feat,lucky).
choice(race(human(variant)),skill,history).
choice(init,'initial class',rogue).
choice(init,background,archaeologist).
choice(initial_class(rogue),skill,[stealth,'sleight of hand',perception,investigation]).
choice(match_class(rogue:1),expertise,[stealth,investigation]).
choice(match_class(rogue:3),subclass,'arcane trickster').
choice(match_class(rogue:4),'asi or feat',[dex,dex]).
choice(match_class(rogue('arcane trickster'):3),cantrip,['minor illusion','booming blade']).
choice(match_class(rogue('arcane trickster'):3),'unconstrained spell','find familiar').
choice(match_class(rogue('arcane trickster'):3),'illusion or enchantment',['disguise self','silent image']).
choice(match_class(rogue('arcane trickster'):4),'illusion or enchantment','charm person').
