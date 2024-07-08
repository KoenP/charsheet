name('monk 5 fighter 11').
base_ability(str,14).
base_ability(dex,18).
base_ability(con,12).
base_ability(int,10).
base_ability(wis,16).
base_ability(cha,8).
gain_level(2,monk,hp_avg).
gain_level(3,monk,hp_avg).
gain_level(4,monk,hp_avg).
gain_level(5,monk,hp_avg).
gain_level(6,fighter,hp_avg).
gain_level(7,fighter,hp_avg).
gain_level(8,fighter,hp_avg).
gain_level(9,fighter,hp_avg).
gain_level(10,fighter,hp_avg).
gain_level(11,fighter,hp_avg).
gain_level(12,fighter,hp_avg).
gain_level(13,fighter,hp_avg).
gain_level(14,fighter,hp_avg).
gain_level(15,fighter,hp_avg).
gain_level(16,fighter,hp_avg).
choice(init,'base race',human).
choice(init,'initial class',monk).
choice(^monk,skill,[acrobatics,insight]).
choice(race(human),subrace,standard).
choice(race(human),language,dwarvish).
choice(init,background,archaeologist).
choice(background(archaeologist),language,goblin).
has(plate+1).
has(greataxe+1).
has(club+2).
