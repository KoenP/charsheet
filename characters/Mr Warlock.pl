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
choice(match_class(warlock:1),subclass,fiend).
choice(match_class(warlock:1),cantrip,['eldritch blast','blade ward']).
choice(match_class(warlock:2),'eldritch invocation',['beast speech','agonizing blast']).
choice(match_class(warlock:2),spell,'hold monster').
choice(match_class(warlock:3),replace('eldritch invocation'),'beast speech').
choice(match_class(warlock:3),replacing('eldritch invocation','beast speech'),'beguiling influence').
choice(match_class(warlock:3),'pact boon',chain).
choice(match_class(warlock:4),replace('eldritch invocation'),'beguiling influence').
choice(match_class(warlock:4),replacing('eldritch invocation','beguiling influence'),'bewitching whispers').
choice(initial_class(warlock),skill,[arcana,deception]).
choice(match_class(wizard:1),'free spell',['absorb elements','color spray','earth tremor']).
choice(match_class(warlock),spell,['armor of agathys']).
choice(match_class(rogue:3),subclass,'arcane trickster').
choice(match_class(warlock:13),replace('eldritch invocation'),['bewitching whispers']).
choice(match_class(warlock:13),'arcanum spell','conjure celestial').
choice(match_class(warlock:13),spell,'comprehend languages').
choice(match_class(warlock:13),replace(spell),'hold monster').
choice(match_class(warlock:13),replacing('eldritch invocation','bewitching whispers'),'repelling blast').
choice(match_class(warlock:13),replacing(spell,'hold monster'),counterspell).
choice(match_class(sorcerer:2),spell,catapult).
choice(class(sorcerer),cantrip,['acid splash','control flames',friends,'green-flame blade']).
choice(class(sorcerer),spell,['burning hands','detect magic']).
choice(match_class(sorcerer:1),subclass,'draconic bloodline').
choice(match_class(warlock:12),'eldritch invocation','armor of shadows').
choice(match_class(warlock:12),replace('eldritch invocation'),['agonizing blast']).
choice(match_class(warlock:12),replace(spell),'hold monster').
choice(match_class(warlock:12),'asi or feat',[cha,cha]).
choice(match_class(warlock:12),replacing('eldritch invocation','agonizing blast'),'repelling blast').
choice(match_class(warlock:12),replacing(spell,'hold monster'),'contact other plane').
