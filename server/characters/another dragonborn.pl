name('another dragonborn').
base_ability(str,10).
base_ability(dex,10).
base_ability(con,10).
base_ability(wis,10).
base_ability(int,10).
base_ability(cha,10).
gain_level(2,bard,hp_avg).
gain_level(3,barbarian,hp_avg).
gain_level(4,barbarian,hp_avg).
gain_level(5,paladin,hp_avg).
choice(init,background,acolyte).
choice(init,'initial class',barbarian).
choice(init,'base race',dragonborn).
choice(^barbarian,skill,['animal handling',nature]).
choice(race(dragonborn),'draconic ancestry',blue).
choice(background(acolyte),language,dwarvish).
choice(bard>:1,'musical instrument',lute).
choice(bard>:1,skill,acrobatics).
choice(bard>:1,spell,['animal friendship']).
choice(bard>:1,cantrip,['dancing lights']).
choice(barbarian>:3,subclass,berserker).
