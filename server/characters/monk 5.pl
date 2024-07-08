name('monk 5').
base_ability(cha,10).
base_ability(con,12).
base_ability(dex,16).
base_ability(int,12).
base_ability(str,9).
base_ability(wis,16).
gain_level(2,monk,hp_avg).
gain_level(3,monk,hp_avg).
gain_level(4,monk,hp_avg).
gain_level(5,monk,hp_avg).
choice(init,'initial class',monk).
choice(monk>:3,subclass,'open hand').
choice(monk>:4,'asi or feat',[dex,dex]).
choice(init,'base race',human).
choice(init,background,outlander).
choice(race(human),subrace,standard).
choice(race(human),language,dwarvish).
choice(^monk,skill,[acrobatics,history]).
