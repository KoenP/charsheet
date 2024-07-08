name(wltest).
base_ability(str,10).
base_ability(dex,10).
base_ability(con,10).
base_ability(wis,10).
base_ability(int,10).
base_ability(cha,10).
choice(init,'base race',human).
choice(race(human),subrace,variant).
choice(init,'initial class',sorcerer).
choice(race(human(variant)),feat,'metamagic adept').
