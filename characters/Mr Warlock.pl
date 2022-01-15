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
choice(_3462,'arcanum spell','wall of ice').
choice(initial_class(warlock),skill,[arcana,deception]).
