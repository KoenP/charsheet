test_char_level(
    'barbarian totem warrior',
    1,
    [ choice(init, 'base race', human),
      choice(init, 'initial class', barbarian),
      asserted_has(greataxe),

      base_ability(str,16),
      base_ability(dex,14),
      base_ability(con,14),
      base_ability(int,12),
      base_ability(wis,10),
      base_ability(cha,8),

      name('barbarian totem warrior')
    ],
    []
).

test_char_level(
    'barbarian totem warrior',
    2,
    [ choice(level(2), 'as class', barbarian),
      choice(level(2), 'max hp roll'(_,_), 7)
    ],
    []
).

test_char_level(
    'barbarian totem warrior',
    3,
    [ choice(level(3), 'as class', barbarian),
      choice(level(3), 'max hp roll'(_,_), 7),
      choice(barbarian >: 3, subclass, 'totem warrior'),
      choice(barbarian('totem warrior') >: 3, 'totem spirit', bear)
    ],
    []
).

test_char_level(
    'barbarian totem warrior',
    L,
    [ choice(level(L), 'as class', barbarian),
      choice(level(L), 'max hp roll'(_,_), 7)
    ],
    []
) :- between(4, 5, L) ; between(7, 9, L).

test_char_level(
    'barbarian totem warrior',
    6,
    [ choice(level(6), 'as class', barbarian),
      choice(level(6), 'max hp roll'(_,_), 7),
      choice(barbarian('totem warrior') >: 6, 'aspect of the beast', eagle)
    ],
    []
).

test_char_level(
    'barbarian totem warrior',
    10,
    [ choice(level(10), 'as class', barbarian),
      choice(level(10), 'max hp roll'(_,_), 7)
    ],
    [ known_spell(barbarian('totem warrior'), 'commune with nature'),
      known_spell_effect(
          barbarian('totem warrior'),
          'commune with nature',
          "A spiritual bear or eagle appears to you to convey the information you seek"
      )
    ]
).
