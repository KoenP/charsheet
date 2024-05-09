name('Tristan Harpell').
base_ability(dex,12).
base_ability(con,12).
base_ability(wis,12).
base_ability(cha,12).
base_ability(int,18).
base_ability(str,16).
gain_level(2,wizard,hp_avg).
gain_level(3,wizard,hp_avg).
gain_level(4,wizard,hp_avg).
gain_level(5,wizard,hp_avg).
gain_level(6,wizard,hp_avg).
gain_level(7,wizard,hp_avg).
gain_level(8,wizard,hp_avg).
gain_level(9,wizard,hp_avg).
gain_level(10,wizard,hp_avg).
gain_level(11,wizard,hp_avg).
gain_level(12,wizard,hp_avg).
gain_level(13,wizard,hp_avg).
gain_level(14,wizard,hp_avg).
gain_level(15,wizard,hp_avg).
gain_level(16,wizard,hp_avg).
gain_level(17,wizard,hp_avg).
gain_level(18,wizard,hp_avg).
gain_level(19,warlock,hp_avg).
gain_level(20,warlock,hp_avg).
choice(init,'base race',human).
choice(race(human),subrace,variant).
choice(race(human(variant)),skill,history).
choice(race(human(variant)),asi,[int+1,con+1]).
choice(init,'initial class',wizard).
choice(wizard>:2,subclass,evocation).
choice(init,background,sage).
choice(race(human(variant)),feat,durable).
choice(race(human),language,giant).
choice(wizard>:18,spell_mastery(1),alarm).
