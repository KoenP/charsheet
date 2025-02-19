test_char_level(
    testpaladin,
    1,
    [choice(init, 'initial class', paladin),
     choice(init, 'base race', tiefling),
     base_ability(str, 18),
     base_ability(dex, 10),
     base_ability(con, 16),
     base_ability(wis, 12),
     base_ability(int, 8),
     base_ability(cha, 16),
     asserted_has(greataxe+1),
     name(testpaladin)],
    [\+ spell_slots(_,_),
     attack(greataxe+1, feet(5), to_hit(7), [damage(slashing, 1 d 12 + 5)], [heavy, twohanded]),
     known_spell(tiefling, thaumaturgy),
     \+ known_spell(paladin, _)]
).

test_char_level(
    testpaladin,
    2,
    [choice(level(2), 'as class', paladin),
     choice(level(2), 'max hp roll'(_,_), 5),
     choice(paladin >: 2, 'fighting style', 'great weapon fighting')],
    [attack(greataxe+1, feet(5), to_hit(7), [damage(slashing, 1 d 12 + 5)],
            [heavy, twohanded, "may reroll 1 or 2 on a damage die"]),
     max_prepared_spells(paladin, 5), % cha mod + half paladin level
     \+ (known_spell(paladin, Spell), spell_property(Spell, level, 0)), % don't know cantrips as paladin
     \+ known_spell(_, 'find steed'), % don't know higher level paladin spells yet
     known_spell(paladin, 'cure wounds')] % know level-appropriate paladin spells
).

test_char_level(
    testpaladin,
    3,
    [choice(level(3), 'as class', paladin),
     choice(level(3), 'max hp roll'(_,_), 5),
     choice(paladin >: 3, subclass, devotion)],
    [known_spell(paladin(devotion), cha, always, [slot], no, sanctuary),
     known_spell(paladin(devotion), cha, always, [slot], no, 'protection from evil and good'),
     \+ known_spell(_, 'lesser restoration'),
     known_spell(tiefling, 'hellish rebuke'),
     res('channel divinity', 1),
     trait(channel_divinity('sacred weapon')),
     trait(channel_divinity('turn the unholy'))]
).

test_char_level(testpaladin, L, [choice(level(L), 'as class', paladin),
     choice(level(L), 'max hp roll'(_,_), 5)], []) :-
    between(4, 10, L).

test_char_level(
    testpaladin,
    11,
    [choice(level(11), 'as class', paladin),
     choice(level(11), 'max hp roll'(_,_), 5)],
    [attack(greataxe+1, feet(5), to_hit(9),
            [damage(slashing, 1 d 12 + 5), damage(radiant, 1 d 8)], _)]
).
