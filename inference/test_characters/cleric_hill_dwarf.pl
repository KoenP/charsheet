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
     choice(cleric >: 1, subclass, life),
     choice(^cleric, skill, [medicine, religion]),
     %choice(trait('blessings of knowledge'), skill, [arcana, nature]),
     %choice(trait('blessings of knowledge'), language, [giant, celestial]),
     choice(cleric >: 1, cantrip, ['sacred flame', 'spare the dying', guidance]),

     has('half plate' + 1),
     has(shield)],
    [ability(str,13),
     ability(dex,10),
     ability(con,16), % +2 from dwarf
     ability(int,12),
     ability(wis,17), % +1 from hill dwarf
     ability(cha,9),

     max_hp(12), % = 8 (cleric base hp) + 3 (con) + 1 (dwarven toughness)
     speed(25),
     ac(armor('half plate' + 1), 16, [shield(shield):2]), % = 15 (half plate) + 1 (half plate enchantment) + 2 (shield) + 0 (dex)
     trait('dwarven toughness'),
     trait(sense(darkvision)),
     trait(stonecunning),
     trait(armor(heavy)), % test a life domain feature

     findall(L-N, spell_slots(L,N), [1-2]),
     spell_save_dc(cleric, 13), % = 8 + 3 (wis) + 2 (prof bon)
     spell_attack_modifier(cleric, 5), % = 3 (wis) + 2 (prof bon)
     max_prepared_spells(cleric, 4), % = 3 (wis) + 1 (cleric level)

     attack('sacred flame', feet(60), saving_throw(13,dex), [damage(radiant,1 d 8)], []),

     known_spell(cleric, wis, always, [], no, 'sacred flame'),
     known_spell(cleric, wis, always, [], no, 'spare the dying'),
     known_spell(cleric, wis, always, [], no, guidance),

     % Domain spell are always prepared
     known_spell(cleric(life), wis, always, [slot], no, 'cure wounds'),
     known_spell(cleric(life), wis, always, [slot], no, bless),
     known_spell_property(cleric(life), 'cure wounds', effects,
                          [heal(1 d 8 + 6)]), % = 3 (wis) + 2 + 1 (spell level)

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
     ac(armor('half plate' + 1), 16, [shield(shield):2]), % = 15 (half plate) + 1 (half plate enchantment) + 2 (shield) + 0 (dex)
     findall(L-N, spell_slots(L,N), [1-3]),
     res('channel divinity', 1),
     max_prepared_spells(cleric, 5), % = 3 (wis) + 2 (cleric level)
     trait(channel_divinity('turn undead'))
    ]).

test_char_level(
    chd,
    3,
    [gain_level(3, cleric, hp_avg)],
    [max_hp(30), % = 8 (base) + 2*5 (lvlup) + 3*3 (con) + 3 (dwarven toughness)
     ac(armor('half plate' + 1), 16, [shield(shield):2]), % = 15 (half plate) + 1 (half plate enchantment) + 2 (shield) + 0 (dex)
     findall(L-N, spell_slots(L,N), [1-4, 2-2]),
     max_prepared_spells(cleric, 6), % = 3 (wis) + 3 (cleric level)

     % Test that we have access to some lvl 2 spells.
     known_spell(cleric, wis, 'when prepared', [slot], no, 'continual flame'),
     known_spell(cleric, wis, 'when prepared', [slot], no, 'hold person'),

     % Domain spells are always prepared.
     known_spell(cleric(life), wis, always, [slot], no, 'lesser restoration'),
     known_spell(cleric(life), wis, always, [slot], no, 'spiritual weapon')
    ]).

test_char_level(
    chd,
    4,
    [gain_level(4, cleric, hp_avg),
     choice(cleric >: 4, 'asi or feat', [wis, str]),
     choice(cleric >: 4, cantrip, mending),
     prepare_spell(cleric, 'cure wounds'),
     prepare_spell(cleric, 'prayer of healing')
    ],
    [max_hp(39), % = 8 (base) + 3*5 (lvlup) + 4*3 (con) + 4 (dwarven toughness)
     ac(armor('half plate' + 1), 16, [shield(shield):2]), % = 15 (half plate) + 1 (half plate enchantment) + 2 (shield) + 0 (dex)
     ability(str,14), % +1 from asi
     ability(dex,10),
     ability(con,16), % +2 from dwarf
     ability(int,12),
     ability(wis,18), % +1 from hill dwarf, + 1 from asi
     ability(cha,9),
     findall(L-N, spell_slots(L,N), [1-4, 2-3]),
     max_prepared_spells(cleric, 8), % = 4 (wis) + 4 (cleric level)
     spell_save_dc(cleric, 14), % = 8 + 4 (wis) + 2 (prof bon)
     spell_attack_modifier(cleric, 6), % = 4 (wis) + 2 (prof bon)
     attack('sacred flame', feet(60), saving_throw(14,dex), [damage(radiant,1 d 8)], []),
     known_spell(cleric, wis, always, [], no, mending),
     known_spell_property(cleric(life), 'cure wounds', effects,
                          [heal(1 d 8 + 7)]) % = 4 (wis) + 2 + 1 (spell level)
    ]).

test_char_level(
    chd,
    5,
    [gain_level(5, cleric, hp_avg)],
    [spell_save_dc(cleric, 15), % = 8 + 4 (wis) + 3 (prof bon)
     spell_attack_modifier(cleric, 7), % = 4 (wis) + 3 (prof bon)
     trait(destroy_undead(cr(1/2))),
     attack('sacred flame', feet(60), saving_throw(15,dex), [damage(radiant,2 d 8)], [])
    ]).

test_char_level(
    chd,
    6,
    [gain_level(6, cleric, hp_avg)],
    [known_spell_property(cleric(life), 'cure wounds', effects,
                          [heal(1 d 8 + 7), (target=other) -> self_heal(3)]),
     known_spell_property(cleric, 'prayer of healing', effects,
                          [heal(2 d 8 + 8) upto "6 creatures", (target=other) -> self_heal(4)])
    ]).

test_char_level(chd, L, [gain_level(L, cleric, hp_avg)], []) :-
    between(7,16,L).

test_char_level(
    chd,
    17,
    [gain_level(17, cleric, hp_avg)],
    [known_spell_property(cleric(life), 'cure wounds', effects,
                          [heal(15), % = 8 (supreme healing) + 4 (wis) + 2 + 1 (spell lvl)
                           (target=other) -> self_heal(3)]), 
     known_spell_property(cleric, 'prayer of healing', effects,
                          [heal(24) upto "6 creatures",
                           (target=other) -> self_heal(4)])]).

%test_char_level(
%    chd,
%    7,
%    [gain_level(7, cleric, hp_avg)],
%    []).
%
%test_char_level(
%    chd,
%    8,
%    [gain_level(8, cleric, hp_avg),
%     ignore(cleric >: 8, 'asi or feat')
%    ],
%    []).
%
%test_char_level(
%    chd,
%    9,
%    [gain_level(9, cleric, hp_avg)],
%    []).
%
%test_char_level(
%    chd,
%    10,
%    [gain_level(10, cleric, hp_avg),
%     ignore(cleric >: 10, cantrip)
%    ],
%    []).
    
%
%gain_level(3, cleric, hp_avg).
%
%gain_level(4, cleric, hp_avg).
%choice(cleric >: 4, 'asi or feat', [wis,str]).
%
%gain_level(5, cleric, hp_avg).
%gain_level(6, cleric, hp_avg).
%gain_level(7, cleric, hp_avg).
%
%prepare_spell(cleric, banishment).
