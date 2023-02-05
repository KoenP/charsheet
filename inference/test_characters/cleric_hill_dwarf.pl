test_char_level(
    chd,
    1,
    [name(chd),
     base_ability(str,13),
     base_ability(dex,10),
     base_ability(con,14),
     base_ability(int,12),
     base_ability(wis,16),
     base_ability(cha,9),

     choice(init, background, archaeologist),
     choice(background(archaeologist), language, elvish),

     choice(init, 'base race', dwarf),
     choice(race(dwarf), tool, smith),
     choice(race(dwarf), subrace, 'hill dwarf'),

     choice(init, 'initial class', cleric),
     choice(match_class(cleric:1), subclass, life),
     choice(initial_class(cleric), skill, [medicine, religion]),
     %choice(trait('blessings of knowledge'), skill, [arcana, nature]),
     %choice(trait('blessings of knowledge'), language, [giant, celestial]),
     choice(class(cleric), cantrip, ['sacred flame', 'spare the dying', guidance]),

     equipped('half plate' + 1),
     equipped(shield)],
    [ability(str,13),
     ability(dex,10),
     ability(con,16), % +2 from dwarf
     ability(int,12),
     ability(wis,17), % +1 from hill dwarf
     ability(cha,9),

     max_hp(12), % = 8 (cleric base hp) + 3 (con) + 1 (dwarven toughness)
     speed(25),
     ac(18), % = 15 (half plate) + 1 (half plate enchantment) + 2 (shield) + 0 (dex)
     trait('dwarven toughness'),
     trait(sense(darkvision)),
     trait(stonecunning),

     findall(L-N, spell_slots(L,N), [1-2]),

     known_spell(cleric, wis, always, [], no, 'sacred flame'),
     known_spell(cleric, wis, always, [], no, 'spare the dying'),
     known_spell(cleric, wis, always, [], no, guidance),

     % domain spell
     %known_spell(cleric, wis, always, [slot], no, 'cure wounds'),

     % Clerics know all spells on their spell list; testing just a few here
     known_spell(cleric, wis, 'when prepared', [slot], no, bane),
     known_spell(cleric, wis, 'when prepared', [slot], no, sanctuary)
    ]
).

test_char_level(
    chd,
    2,
    [gain_level(2, cleric, hp_avg)],
    [max_hp(21), % = 8 (base) + 1*5 (lvlup) + 2*3 (con) + 2 (dwarven toughness)
     ac(18),
     findall(L-N, spell_slots(L,N), [1-3]),
     resource('channel divinity', 'channel divinity', 2),
     trait(channel_divinity('turn undead'))
    ]).

test_char_level(
    chd,
    3,
    [gain_level(3, cleric, hp_avg)],
    [max_hp(30), % = 8 (base) + 2*5 (lvlup) + 3*3 (con) + 3 (dwarven toughness)
     ac(18),
     findall(L-N, spell_slots(L,N), [1-4, 2-2]),

     % Test that we have access to some lvl 2 spells
     known_spell(cleric, wis, 'when prepared', [slot], no, 'continual flame'),
     known_spell(cleric, wis, 'when prepared', [slot], no, 'hold person')
    ]).

test_char_level(
    chd,
    4,
    [gain_level(4, cleric, hp_avg),
     choice(match_class(cleric:4), 'asi or feat', [wis, str]),
     choice(match_class(cleric:4), cantrip, mending),
     prepare_spell(cleric, 'cure wounds')
    ],
    [max_hp(39), % = 8 (base) + 3*5 (lvlup) + 4*3 (con) + 4 (dwarven toughness)
     ac(18),
     ability(str,14), % +1 from asi
     ability(dex,10),
     ability(con,16), % +2 from dwarf
     ability(int,12),
     ability(wis,18), % +1 from hill dwarf, + 1 from asi
     ability(cha,9),
     findall(L-N, spell_slots(L,N), [1-4, 2-3]),
     known_spell(cleric, wis, always, [], no, mending)
    ]).
    
%
%gain_level(3, cleric, hp_avg).
%
%gain_level(4, cleric, hp_avg).
%choice(match_class(cleric:4), 'asi or feat', [wis,str]).
%
%gain_level(5, cleric, hp_avg).
%gain_level(6, cleric, hp_avg).
%gain_level(7, cleric, hp_avg).
%
%prepare_spell(cleric, banishment).
