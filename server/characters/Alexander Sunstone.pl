name("Alexander Sunstone").
base_ability(str,9).
base_ability(dex,13).
base_ability(con,15).
base_ability(int,11).
base_ability(wis,9).
base_ability(cha,16).
gain_level(2,warlock,hp_avg).
gain_level(3,warlock,hp_avg).
choice(init,'base race',human).
choice(race(human),language,elvish).
choice(race(human),subrace,variant).
choice(race(human(variant)),asi,[con+1,dex+1]).
choice(race(human(variant)),skill,persuasion).
choice(race(human(variant)),feat,metamagic_adept('careful spell','empowered spell')).
choice(init,'initial class',warlock).
choice(warlock>:1,subclass,fiend).
choice(init,background,sage).
choice(warlock>:1,cantrip,['eldritch blast','minor illusion']).
choice(warlock>:1,spell,[hex,'burning hands']).
choice(^warlock,skill,[deception,intimidation]).
choice(warlock>:2,'eldritch invocation',['agonizing blast','mask of many faces']).
choice(warlock>:2,spell,'charm person').
choice(warlock>:3,spell,shatter).
choice(warlock>:3,'pact boon',chain).
choice(warlock>:3,replace(spell),'burning hands').
choice(warlock>:3,replacing(spell,'burning hands'),suggestion).
has('studded leather').
