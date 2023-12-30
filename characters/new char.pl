name('new char').
base_ability(str,10).
base_ability(dex,10).
base_ability(con,10).
base_ability(wis,10).
base_ability(int,10).
base_ability(cha,10).
choice(init,'initial class',paladin).
choice(init,'base race',dragonborn).
choice(init,background,archaeologist).
choice(race(dragonborn),'draconic ancestry',blue).
