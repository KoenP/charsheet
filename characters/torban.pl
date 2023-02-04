name('Torban the Tenacious').
base_ability(str,13).
base_ability(dex,10).
base_ability(con,14).
base_ability(int,12).
base_ability(wis,16).
base_ability(cha,9).
choice(init, background, archaeologist).
choice(init, 'base race', dwarf).
choice(race(dwarf), tool, smith).
choice(race(dwarf), subrace, 'hill dwarf').
choice(init, 'initial class', cleric).
choice(match_class(cleric:1), subclass, knowledge).
choice(initial_class(cleric), skill, [medicine, religion]).
%choice(trait('blessings of knowledge'), language, [elvish, giant]).
choice(trait('blessings of knowledge'), skill, [arcana, nature]).
choice(class(cleric), cantrip, ['sacred flame', 'spare the dying', guidance]).

gain_level(2, cleric, hp_avg).

gain_level(3, cleric, hp_avg).

gain_level(4, cleric, hp_avg).
choice(match_class(cleric:4), 'asi or feat', [wis,str]).

gain_level(5, cleric, hp_avg).
gain_level(6, cleric, hp_avg).
gain_level(7, cleric, hp_avg).

prepare_spell(cleric, banishment).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% might not be correct
%spell_auto_data('toll the dead',
%                properties{
%                    area_of_effect:false,
%                    attack_type:false,
%                    casting_time:"1 action",
%                    classes:[cleric],
%                    components:[v, s, m("Powdered rhubarb leaf and an adder's stomach.")],
%                    concentration:false,
%                    damage_at_slot_level:_{2:damage(acid, 4 d 4), 3:damage(acid, 5 d 4), 4:damage(acid, 6 d 4), 5:damage(acid, 7 d 4), 6:damage(acid, 8 d 4), 7:damage(acid, 9 d 4), 8:damage(acid, 10 d 4), 9:damage(acid, 11 d 4)},
%                    damage_with_cantrip_scaling:false,
%                    dc:false,
%                    desc:["A shimmering green arrow streaks toward a target within range and bursts in a spray of acid. Make a ranged spell attack against the target. On a hit, the target takes 4 d 4 acid damage immediately and 2 d 4 acid damage at the end of its next turn. On a miss, the arrow splashes the target with acid for half as much of the initial damage and no damage at the end of its next turn."],
%                    duration:"Instantaneous",
%                    higher_level:"When you cast this spell using a spell slot of 3rd level or higher, the damage (both initial and later) increases by 1 d 4 for each slot level above 2nd.",
%                    level:2,
%                    range:feet(90),
%                    ritual:false,
%                    school:evocation}}).
