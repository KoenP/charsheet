name('Mr Warlock').
base_ability(str,12).
base_ability(dex,12).
base_ability(con,12).
base_ability(wis,12).
base_ability(cha,18).
base_ability(int,12).
gain_level(2,warlock,hp_avg).
gain_level(3,warlock,hp_avg).
gain_level(4,warlock,hp_avg).
gain_level(5,warlock,hp_avg).
gain_level(6,warlock,hp_avg).
gain_level(7,warlock,hp_avg).
gain_level(8,warlock,hp_avg).
gain_level(9,warlock,hp_avg).
gain_level(10,warlock,hp_avg).
gain_level(11,warlock,hp_avg).
gain_level(12,wizard,hp_avg).
gain_level(13,gambler,hp_avg).
gain_level(14,rogue,hp_avg).
gain_level(15,warlock,hp_avg).
gain_level(16,sorcerer,hp_avg).
gain_level(17,sorcerer,hp_avg).
gain_level(18,warlock,hp_avg).
gain_level(19,rogue,hp_avg).
gain_level(20,rogue,hp_avg).
choice(init,'base race',elf).
choice(race(elf),subrace,'high elf').
choice(init,'initial class',warlock).
choice(warlock>:2,'eldritch invocation',['agonizing blast','armor of shadows']).
choice(warlock>:3,replace('eldritch invocation'),['agonizing blast']).
choice(warlock>:3,replacing('eldritch invocation','agonizing blast'),'beguiling influence').
choice(warlock>:4,'asi or feat','metamagic adept').
