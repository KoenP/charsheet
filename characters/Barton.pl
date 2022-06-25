name('Barton').
base_ability(str,10).
base_ability(wis,10).
base_ability(int,10).
base_ability(cha,10).
base_ability(con,14).
base_ability(dex,15).
gain_level(2,rogue,hp_avg).
gain_level(3,rogue,hp_avg).
gain_level(4,rogue,hp_avg).
choice(init,'base race',human).
choice(init,'initial class',rogue).
choice(init,background,archaeologist).
choice(background(archaeologist),language,gnomish).
choice(race(human),subrace,variant).
choice(race(human(variant)),feat,lucky).
choice(race(human(variant)),asi,[dex+1,dex+1]).
choice(initial_class(rogue),skill,[acrobatics,investigation,perception,stealth]).
choice(match_class(rogue:1),expertise,[stealth,perception]).
choice(race(human),language,elvish).
choice(match_class(rogue:3),subclass,'arcane trickster').
choice(match_class(rogue:4),'asi or feat',[dex,dex]).
