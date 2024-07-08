name(nyx).
base_ability(str,10).
base_ability(dex,10).
base_ability(con,10).
base_ability(wis,10).
base_ability(int,10).
base_ability(cha,10).
gain_level(2,rogue,hp_avg).
gain_level(3,rogue,hp_avg).
gain_level(4,rogue,hp_avg).
choice(init,'initial class',rogue).
choice(init,background,archaeologist).
choice(init,'base race',human).
choice(match_class(rogue:4),'asi or feat',durable).
choice(initial_class(rogue),skill,[insight,investigation]).
