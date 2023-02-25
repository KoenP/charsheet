test_char_level(
    drelf,
    1,
    [name(drelf),
     base_ability(str,10),
     base_ability(dex,13),
     base_ability(con,14),
     base_ability(int,12),
     base_ability(wis,16),
     base_ability(cha,9),

     equipped(quarterstaff),

     choice(init, 'base race', elf),
     choice(race(elf), subrace, 'high elf'),
     choice(race(elf('high elf')), cantrip, 'fire bolt'),

     choice(init, 'initial class', druid),
     choice(class(druid), cantrip, [shillelagh, druidcraft]),
     choice(initial_class(druid), skill, [arcana, 'animal handling'])
    ],
    [max_hp(10), % 8 (base druid) + 2 (con mod)
     ac(12), % 10 + 2 (dex mod)
     ability(str,10),
     ability(dex,15), % elf bonus + 2
     ability(con,14),
     ability(int,13), % elf bonus + 1
     ability(wis,16),
     ability(cha,9),
     attack_variant(quarterstaff:shillelagh, feet(5), to_hit(5),
                    [damage(bludgeoning, 1 d 8 + 3)], [magical]),
     attack_variant(quarterstaff:twohanded, feet(5), to_hit(2),
                    [damage(bludgeoning, 1 d 8)], []),
     attack(quarterstaff, feet(5), to_hit(2), [damage(bludgeoning, 1 d 6)], _),

     known_spell(race(elf('high elf')), int, always, [], no, 'fire bolt'),
     known_spell(druid, wis, always, [], no, shillelagh),
     known_spell(druid, wis, always, [], no, druidcraft)]).

test_char_level(
    drelf,
    2,
    [gain_level(2, druid, hp_avg)],
    []).
