test_char_level(
    infusions,
    1,
    [ name(infusions),
      base_ability(int, 16),
      base_ability(con, 14),
      base_ability(str, 15),
      base_ability(cha, 10),
      base_ability(wis, 11),
      base_ability(dex, 8),

      choice(init, 'initial class', artificer)
    ],
    []).

test_char_level(
    infusions,
    2,
    [ choice(level(2), 'as class', artificer),
      choice(level(2), 'max hp roll'(_,_), 5),
      choice(artificer >: 2, infusion, ['repeating shot']),
      choice(trait(infusion('repeating shot')),
             'equip repeating shot weapon',
             'light crossbow')
    ],
    [ attack('light crossbow'+1 ~ 'repeating shot',
             feet(80)/feet(320),
             to_hit(2),
             [damage(piercing, 1 d 8)],
             [ammunition, twohanded]) % loading should be omitted
    ]).

test_char_level(infusions, L,
                [ choice(level(L), 'as class', artificer),
                  choice(level(L), 'max hp roll'(_,_), 5)
                ],
                []) :-
    between(3, 14, L).

test_char_level(
    infusions, 15,
    [ choice(level(15), 'as class', artificer),
      choice(level(15), 'max hp roll'(_,_), 5),
      choice(artificer >: 14, infusion, ['enhanced weapon', 'enhanced defense']),
      choice(trait(infusion('enhanced weapon')), 'equip enhanced weapon', quarterstaff),
      choice(trait(infusion('enhanced defense')), 'equip enhanced defense armor', plate)
    ],
    [ ac(20),
      attack(quarterstaff+2 ~ 'enhanced weapon', feet(5), to_hit(ToHit), [damage(bludgeoning, 1 d 6 + DamageBonus)], [versatile(1 d 8)]),
      inferred_has(plate+2 ~ 'enhanced defense'),
      inferred_has(quarterstaff+_ ~ 'enhanced weapon')
    ]) :-
    ProfBon = 5, StrMod = 2, Enchantment = 2,
    ToHit is ProfBon + StrMod + Enchantment, % +9
    DamageBonus is StrMod + Enchantment. % +4


test_char_level(infusions, 16,
                [ choice(level(16), 'as class', artificer),
                  choice(level(16), 'max hp roll'(_,_), 5),
                  choice(artificer >: 16, replace(infusion), 'enhanced weapon')
                ],
    [ \+ inferred_has(quarterstaff+_ ~'enhanced weapon'),
      inferred_has(plate+2 ~ 'enhanced defense')
    ],
    [not_eligible(trait(infusion('enhanced weapon')), 'equip enhanced weapon', quarterstaff)]).
