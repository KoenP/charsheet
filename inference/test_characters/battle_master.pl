test_char_level(
    battlemaster,
    1,
    [ name(battlemaster),
      base_ability(str,16),
      base_ability(dex,15),
      base_ability(con,14),
      base_ability(int,13),
      base_ability(wis,12),
      base_ability(cha,11),

      choice(init, 'initial class', fighter)
    ],
    []
).

test_char_level(
    battlemaster,
    2,
    [ choice(level(2), 'as class', fighter),
      choice(level(2), 'max hp roll'(_,_), 6)
    ],
    []
).

test_char_level(
    battlemaster,
    3,
    [ choice(level(3), 'as class', fighter),
      choice(level(3), 'max hp roll'(_,_), 6),
      choice(fighter >: 3, subclass, 'battle master'),
      choice(fighter('battle master') >: 3, maneuver, [riposte, ambush, 'bait and switch'])
    ],
    [ trait(maneuver(riposte)),
      trait(maneuver(ambush)),
      trait(maneuver('bait and switch')),
      trait('combat superiority'(4 d 8, SaveDC))
    ]
) :- SaveDC is 8 + 2 + 3.

test_char_levelup_fromto(battlemaster, fighter, 4-6).

test_char_level(
    battlemaster,
    7,
    [ choice(level(7), 'as class', fighter),
      choice(level(7), 'max hp roll'(_,_), 6)
    ],
    [ class_level(fighter:7),
      trait('combat superiority'(5 d 8, SaveDC))
    ]
) :- SaveDC is 8 + 3 + 3.


test_char_levelup_fromto(battlemaster, fighter, 8-15).
