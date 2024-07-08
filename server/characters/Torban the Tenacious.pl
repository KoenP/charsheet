name('Torban the Tenacious').
base_ability(str,13).
base_ability(dex,10).
base_ability(con,14).
base_ability(int,12).
base_ability(wis,16).
base_ability(cha,9).
gain_level(2,cleric,hp_avg).
gain_level(3,cleric,hp_avg).
gain_level(4,cleric,hp_avg).
gain_level(5,cleric,hp_avg).
gain_level(6,cleric,hp_avg).
gain_level(7,cleric,hp_avg).
choice(init,background,archaeologist).
choice(init,'base race',dwarf).
choice(race(dwarf),tool,smith).
choice(race(dwarf),subrace,'hill dwarf').
choice(init,'initial class',cleric).
choice(cleric>:4,'asi or feat',[str,dex]).
choice(cleric>:4,cantrip,guidance).
