created(1722857577.1974578).
assert(name(monk,'monk 5 fighter 11')).
assert(base_ability(monk,str,14)).
assert(base_ability(monk,dex,18)).
assert(base_ability(monk,con,12)).
assert(base_ability(monk,int,10)).
assert(base_ability(monk,wis,16)).
assert(base_ability(monk,cha,8)).
assert(gain_level(monk,2,monk,hp_avg)).
assert(gain_level(monk,3,monk,hp_avg)).
assert(gain_level(monk,4,monk,hp_avg)).
assert(gain_level(monk,5,monk,hp_avg)).
assert(gain_level(monk,6,fighter,hp_avg)).
assert(gain_level(monk,7,fighter,hp_avg)).
assert(gain_level(monk,8,fighter,hp_avg)).
assert(gain_level(monk,9,fighter,hp_avg)).
assert(gain_level(monk,10,fighter,hp_avg)).
assert(gain_level(monk,11,fighter,hp_avg)).
assert(gain_level(monk,12,fighter,hp_avg)).
assert(gain_level(monk,13,fighter,hp_avg)).
assert(gain_level(monk,14,fighter,hp_avg)).
assert(gain_level(monk,15,fighter,hp_avg)).
assert(gain_level(monk,16,fighter,hp_avg)).
assert(choice(monk,init,'base race',human)).
assert(choice(monk,init,'initial class',monk)).
assert(choice(monk,^monk,skill,[acrobatics,insight])).
assert(choice(monk,race(human),subrace,standard)).
assert(choice(monk,race(human),language,dwarvish)).
assert(choice(monk,init,background,archaeologist)).
assert(choice(monk,background(archaeologist),language,goblin)).
