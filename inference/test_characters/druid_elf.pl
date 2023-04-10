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

     has(quarterstaff),

     choice(init, 'base race', elf),
     choice(race(elf), subrace, 'high elf'),
     choice(race(elf('high elf')), cantrip, 'fire bolt'),
     choice(race(elf('high elf')), language, gnomish),

     choice(init, 'initial class', druid),
     choice(class(druid), cantrip, [shillelagh, druidcraft]),
     choice(initial_class(druid), skill, [arcana, 'animal handling'])
    ],
    [max_hp(10), % 8 (base druid) + 2 (con mod)
     ac(12), % 10 + 2 (dex mod)
     speed(30),
     ability(str,10),
     ability(dex,15), % elf bonus + 2
     ability(con,14),
     ability(int,13), % high elf bonus + 1
     ability(wis,16),
     ability(cha,9),
     
     % racial traits
     trait(race(elf), sense(darkvision)),
     trait(trait(sense('keen senses')), skill(perception)),
     trait(race(elf('high elf')), weapon(longsword)),
     trait(race(elf('high elf')), weapon(shortsword)),
     trait(race(elf('high elf')), weapon(shortbow)),
     trait(race(elf('high elf')), weapon(longbow)),
     
     \+ resource('wild shape', _, _),

     attack_variant(quarterstaff:shillelagh, feet(5), to_hit(5),
                    [damage(bludgeoning, 1 d 8 + 3)], [magical]),
     attack_variant(quarterstaff:twohanded, feet(5), to_hit(2),
                    [damage(bludgeoning, 1 d 8)], []),
     attack(quarterstaff, feet(5), to_hit(2), [damage(bludgeoning, 1 d 6)], _),

     known_spell(race(elf('high elf')), int, always, [], no, 'fire bolt'),
     known_spell(druid, wis, always, [], no, shillelagh),
     known_spell(druid, wis, always, [], no, druidcraft),

     known_spell(druid, wis, 'when prepared', [slot], no, 'animal friendship'),
     known_spell(druid, wis, 'when prepared', [slot], no, 'charm person'),
     known_spell(druid, wis, 'when prepared', [slot], no, goodberry),

     % don't know any circle spells yet
     \+ known_spell(druid, wis, always, [slot], no, barkskin),
     \+ known_spell(druid, wis, always, [slot], no, 'mirror image'),
     \+ known_spell(druid, wis, always, [slot], no, 'hold person')]).

test_char_level(
    drelf,
    2,
    [gain_level(2, druid, hp_avg),
     choice(druid >: 2,subclass,land),
     choice(match_class(druid(land):2), cantrip, guidance)
    ],
    [max_hp(17), % 8 (base) + 1*5 (levelup) + 2*2 (con mod)

     resource('natural recovery', 'spell slot total', 1),
     resource('wild shape', 'wild shape', 2),
     trait(wild_shape([cr(1/4),hours(1),'no swimming speed','no flying speed'])),

     known_spell(druid, wis, always, [], no, guidance),
     % don't know any circle spells yet
     \+ known_spell(druid, wis, always, [slot], no, barkskin),
     \+ known_spell(druid, wis, always, [slot], no, 'mirror image'),
     \+ known_spell(druid, wis, always, [slot], no, 'hold person')
    ]).

test_char_level(
    drelf,
    3,
    [gain_level(3, druid, hp_avg),
     choice(match_class(druid(land):3),'circle spells',forest)

    ],
    [resource('natural recovery', 'spell slot total', 2),
     resource('wild shape', 'wild shape', 2),
     trait(wild_shape([cr(1/4),hours(1),'no swimming speed','no flying speed'])),

     % know only forest circle spells
     known_spell(druid, wis, always, [slot], no, barkskin),
     \+ known_spell(druid, wis, always, [slot], no, 'mirror image'),
     \+ known_spell(druid, wis, always, [slot], no, 'hold person')
    ]).
