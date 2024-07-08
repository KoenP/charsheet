name('draconic evoker').
base_ability(cha,18).
base_ability(con,11).
base_ability(dex,14).
base_ability(int,14).
base_ability(str,9).
base_ability(wis,12).
gain_level(2,sorcerer,hp_avg).
gain_level(3,sorcerer,hp_avg).
gain_level(4,sorcerer,hp_avg).
gain_level(5,sorcerer,hp_avg).
gain_level(6,sorcerer,hp_avg).
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
choice(init,'initial class',sorcerer).
choice(init,'base race',gnome).
choice(init,background,sage).
choice(race(gnome),subrace,'rock gnome').
choice(race(gnome('rock gnome')),tool,smith).
choice(^sorcerer,skill,[deception,insight]).
choice(sorcerer>:1,cantrip,['chill touch']).
choice(sorcerer>:1,subclass,'draconic bloodline').
choice(sorcerer('draconic bloodline')>:1,'dragon ancestor',red).
choice(sorcerer>:1,spell,['burning hands']).
choice(wizard>:1,cantrip,['fire bolt','acid splash']).
choice(wizard>:1,'free spell',['burning hands',thunderwave]).
choice(wizard>:2,subclass,evocation).
choice(wizard>:2,'free spell',['black tentacles']).
