name('Jehnlana').
base_ability(str,12).
base_ability(dex,12).
base_ability(con,12).
base_ability(wis,12).
base_ability(cha,12).
base_ability(int,12).
gain_level(2,gambler,hp_avg).
gain_level(3,gambler,hp_avg).
gain_level(4,gambler,hp_avg).
gain_level(5,gambler,hp_avg).
choice(init,'base race',human).
choice(race(human),subrace,standard).
choice(init,'initial class',gambler).
choice(initial_class(gambler),skill,['sleight of hand',insight,perception]).
choice(race(human),language,gnomish).
