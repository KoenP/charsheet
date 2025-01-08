test_char_level(
    'human ranger', 1,
    [name('human ranger'),
     base_ability(str,9),
     base_ability(dex,15),
     base_ability(con,11),
     base_ability(int,11),
     base_ability(wis,14),
     base_ability(cha,8),
     choice(init, 'base race', human),
     choice(race(human), subrace, variant),
     choice(race(human), language, 'deep speech'),
     choice(race(human(variant)), skill, acrobatics),
     choice(race(human(variant)), feat, 'fey touched'),
     choice(feat('fey touched'), asi, wis + 1),
     choice(feat('fey touched'), spell, sleep),
     choice(race(human(variant)), asi, [dex + 1, wis + 1]),
     choice(init, 'initial class', ranger)],
    [max_hp(10),
     ability(str,9),
     ability(dex,16),
     ability(con,11),
     ability(int,11),
     ability(wis,16),
     ability(cha,8),
     \+ spell_slots(_,_),
     known_spell('fey touched', sleep),
     known_spell('fey touched', 'misty step')]
).

test_char_level(
    'human ranger',
    2,
    [gain_level(2, ranger, hp_avg),
     choice(ranger >: 2, 'fighting style', archery),
     choice(ranger >: 2, spell, ['hunter\'s mark', 'cure wounds'])],
    [max_hp(16),
     findall(L-N, spell_slots(L,N), [1-2]),
     known_spell(ranger, 'hunter\'s mark'),
     known_spell(ranger, 'cure wounds')]
).
