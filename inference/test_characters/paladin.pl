test_char_level(
    testpaladin,
    1,
    [choice(init, 'initial class', paladin),
     base_ability(str, 18),
     base_ability(dex, 10),
     base_ability(con, 16),
     base_ability(wis, 12),
     base_ability(int, 8),
     base_ability(cha, 16),
     has(greataxe+1),
     name(testpaladin)],
    [\+ spell_slots(_,_),
     attack(greataxe+1, feet(5), to_hit(7), [damage(slashing, 1 d 12 + 5)], [heavy, twohanded]),
     \+ known_spell(_,_)] % no spell slots -> no known spells
).

test_char_level(
    testpaladin,
    2,
    [gain_level(2, paladin, hp_avg),
     choice(paladin >: 2, 'fighting style', 'great weapon fighting')],
    [attack(greataxe+1, feet(5), to_hit(7), [damage(slashing, 1 d 12 + 5)],
            [heavy, twohanded, "may reroll 1 or 2 on a damage die"]),
     max_prepared_spells(paladin, 4), % cha mod + half paladin level
     \+ (known_spell(_, Spell), spell_property(Spell, level, 0)), % don't know cantrips
     \+ known_spell(_, 'find steed'), % don't know higher level paladin spells yet
     known_spell(paladin, 'cure wounds')] % know level-appropriate paladin spells
).

test_char_level(
    testpaladin,
    3,
    [gain_level(3, paladin, hp_avg),
     choice(paladin >: 3, subclass, devotion)],
    [known_spell(paladin(devotion), sanctuary),
     known_spell(paladin(devotion), 'protection from evil and good'),  
     \+ known_spell(_, 'lesser restoration'),  
     resource('channel divinity', 'channel divinity', 1),
     trait(channel_divinity('sacred weapon')),
     trait(channel_divinity('turn the unholy'))]
).

test_char_level(testpaladin, L, [gain_level(L, paladin, hp_avg)], []) :-
    between(4, 10, L).

test_char_level(
    testpaladin,
    11,
    [gain_level(11, paladin, hp_avg)],
    [attack(greataxe+1, feet(5), to_hit(9),
            [damage(slashing, 1 d 12 + 5), damage(radiant, 1 d 8)], _)]
).
