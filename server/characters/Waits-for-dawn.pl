name('Waits-for-dawn').
base_ability(cha,9).
base_ability(con,15).
base_ability(dex,12).
base_ability(int,13).
base_ability(str,13).
base_ability(wis,16).
gain_level(2,cleric,hp_avg).
choice(init,background,acolyte).
choice(init,'initial class',cleric).
choice(init,'base race',tabaxi).
choice(race(tabaxi),'ability + 2',wis+2).
choice(race(tabaxi),'ability + 1',str+1).
choice(race(tabaxi),language,dwarvish).
choice(background(acolyte),language,giant).
choice(^cleric,skill,[medicine,history]).
choice(cleric>:1,subclass,life).
choice(cleric>:1,cantrip,[guidance,'sacred flame','spare the dying']).
has('chain mail').
has(shield).
has(mace).
has(javelin).
