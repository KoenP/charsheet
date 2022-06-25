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
choice(match_class(wizard:2),subclass,evocation).
choice(match_class(wizard:2),'free spell',['burning hands','comprehend languages']).
choice(match_class(wizard:3),'free spell',[shatter,'see invisibility']).
choice(match_class(wizard:4),cantrip,'fire bolt').
choice(match_class(wizard:4),'free spell',[darkvision,'misty step']).
choice(match_class(wizard:5),'free spell',[counterspell,fireball]).
choice(match_class(wizard:6),'free spell',[slow,'stinking cloud']).
choice(match_class(warlock:1),cantrip,['eldritch blast','blade ward']).
choice(match_class(warlock:2),'eldritch invocation',['mask of many faces','agonizing blast']).
choice(match_class(wizard:18),spell_mastery(1),thunderwave).
choice(match_class(wizard:18),spell_mastery(2),shatter).
choice(match_class(wizard:4),'asi or feat',alert).
choice(match_class(wizard:1),cantrip,['blade ward','dancing lights',message]).
choice(match_class(wizard:1),'free spell',['abi-dalzim\'s horrid wilting','aganazzar\'s scorcher','arcane gate']).
choice(initial_class(wizard),skill,[medicine]).
choice(match_class(wizard:16),'free spell',['arcane eye','antipathy/sympathy']).
choice(match_class(wizard:16),'asi or feat',[cha]).
