name('Zaphod Beeblebrox').
base_ability(str,10).
base_ability(dex,10).
base_ability(con,10).
base_ability(wis,10).
base_ability(int,10).
base_ability(cha,10).
choice(init,'base race',elf).
choice(init,background,acolyte).
choice(init,'initial class',barbarian).
choice(race(elf),subrace,'high elf').
choice(race(elf('high elf')),language,giant).
choice(race(elf('high elf')),cantrip,'acid splash').
choice(background(acolyte),language,halfling).
choice(^barbarian,skill,['animal handling',athletics]).
