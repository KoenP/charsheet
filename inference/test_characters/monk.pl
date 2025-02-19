:- dynamic trait/1.

test_char_level(
    monk,
    1,
    [choice(init, 'base race', 'half-orc'),
     choice(init, 'initial class', monk),
     asserted_has(club),
     asserted_has(quarterstaff),

     base_ability(str,14),
     base_ability(dex,18),
     base_ability(con,12),
     base_ability(int,10),
     base_ability(wis,16),
     base_ability(cha,8),

     name(monk)],
    [speed(30),
     attack_with_sorted_notes(club, feet(5), to_hit(6), [damage(bludgeoning, 1 d 4 + 4)],
            ["unarmed strike as bonus action", light]),
     attack_with_sorted_notes(unarmed, feet(5), to_hit(6), [damage(bludgeoning, 1 d 4 + 4)],
            ["unarmed strike as bonus action"]),
     attack_with_sorted_notes(quarterstaff, feet(5), to_hit(6), [damage(bludgeoning, 1 d 6 + 4)],
            ["unarmed strike as bonus action", versatile(1 d 8)]),
     attack_variant(quarterstaff:twohanded, feet(5), to_hit(6), [damage(bludgeoning, 1 d 8 + 4)],
            []),
     ac(unarmored_defense(monk), 17, [])]
).

test_char_level(
    monk,
    2,
    [choice(level(2), 'as class', fighter),
     choice(level(2), 'max hp roll'(_,_), 5), % a level of fighter to get shield proficiency
     asserted_has(shield) % shouldn't show up in unarmored defense
    ],
    [attack_with_sorted_notes(club, feet(5), to_hit(6), [damage(bludgeoning, 1 d 4 + 4)],
            ["unarmed strike as bonus action", light]),
     attack_with_sorted_notes(unarmed, feet(5), to_hit(6), [damage(bludgeoning, 1 d 4 + 4)],
            ["unarmed strike as bonus action"]),
     attack_with_sorted_notes(quarterstaff, feet(5), to_hit(6), [damage(bludgeoning, 1 d 6 + 4)],
            ["unarmed strike as bonus action", versatile(1 d 8)]),
     attack_variant(quarterstaff:twohanded, feet(5), to_hit(6), [damage(bludgeoning, 1 d 8 + 4)],
            []),
     ac(unarmored_defense(monk), 17, [])]
).

test_char_level(
    monk,
    3,
    [choice(level(3), 'as class', monk),
     choice(level(3), 'max hp roll'(_,_), 5) % a level of fighter to get shield proficiency
    ],
    [res('ki points', 2),
     speed(40),
     attack_with_sorted_notes(club, feet(5), to_hit(6), [damage(bludgeoning, 1 d 4 + 4)],
            ["unarmed strike as bonus action", light]),
     attack_with_sorted_notes(unarmed, feet(5), to_hit(6), [damage(bludgeoning, 1 d 4 + 4)],
            ["unarmed strike as bonus action"]),
     attack_with_sorted_notes(quarterstaff, feet(5), to_hit(6), [damage(bludgeoning, 1 d 6 + 4)],
            ["unarmed strike as bonus action", versatile(1 d 8)]),
     attack_variant(quarterstaff:twohanded, feet(5), to_hit(6), [damage(bludgeoning, 1 d 8 + 4)],
            []),
     ac(unarmored_defense(monk), 17, [])]
).

test_char_level(
    monk,
    4,
    [choice(level(4), 'as class', monk),
     choice(level(4), 'max hp roll'(_,_), 5)],
    [res('ki points', 3),
     speed(40),
     attack_with_sorted_notes(club, feet(5), to_hit(6), [damage(bludgeoning, 1 d 4 + 4)],
            ["unarmed strike as bonus action", light]),
     attack_with_sorted_notes(unarmed, feet(5), to_hit(6), [damage(bludgeoning, 1 d 4 + 4)],
            ["unarmed strike as bonus action"]),
     attack_with_sorted_notes(quarterstaff, feet(5), to_hit(6), [damage(bludgeoning, 1 d 6 + 4)],
            ["unarmed strike as bonus action", versatile(1 d 8)]),
     attack_variant(quarterstaff:twohanded, feet(5), to_hit(6), [damage(bludgeoning, 1 d 8 + 4)],
            []),
     ac(unarmored_defense(monk), 17, [])]
).

test_char_level(
    monk,
    5,
    [choice(level(5), 'as class', monk),
     choice(level(5), 'max hp roll'(_,_), 5),
     choice(monk >: 4, 'asi or feat', alert)],
    [feat(alert),
     initiative(9) % dex mod (+4) + alert feat (+5)
    ]).

test_char_level(
    monk, 6, [choice(level(6), 'as class', monk),
     choice(level(6), 'max hp roll'(_,_), 5)],
    [ choice(level(6), 'as class', monk),
     choice(level(6), 'max hp roll'(_,_), 5),
      res('ki points', 5),
      speed(40),
      attack_with_sorted_notes(club, feet(5), to_hit(7), [damage(bludgeoning, 1 d 6 + 4)],
             ["attack 2x", "unarmed strike as bonus action", light]),
      attack_with_sorted_notes(unarmed, feet(5), to_hit(7), [damage(bludgeoning, 1 d 6 + 4)],
             ["attack 2x", "unarmed strike as bonus action"]),
      attack_with_sorted_notes(quarterstaff, feet(5), to_hit(7), [damage(bludgeoning, 1 d 6 + 4)],
             ["attack 2x", "unarmed strike as bonus action", versatile(1 d 8)]),
      attack_variant(quarterstaff:twohanded, feet(5), to_hit(7),
                     [damage(bludgeoning, 1 d 8 + 4)],
                     []),
      ac(unarmored_defense(monk), 17, [])
    ]).

test_char_level(monk, L, [choice(level(L), 'as class', monk),
     choice(level(L), 'max hp roll'(_,_), 5)], []) :-
    between(7,17,L).

% monk level 17
test_char_level(
    monk, 18, [choice(level(18), 'as class', monk),
     choice(level(18), 'max hp roll'(_,_), 5)],
    [ choice(level(18), 'as class', monk),
     choice(level(18), 'max hp roll'(_,_), 5),
      res('ki points', 17),
      speed(55),
      attack_with_sorted_notes(club, feet(5), to_hit(10), [damage(bludgeoning, 1 d 10 + 4)],
             ["attack 2x", "unarmed strike as bonus action", light]),
      attack_with_sorted_notes(unarmed, feet(5), to_hit(10), [damage(bludgeoning, 1 d 10 + 4)],
             ["attack 2x", "unarmed strike as bonus action", magical]),
      attack_with_sorted_notes(quarterstaff, feet(5), to_hit(10), [damage(bludgeoning, 1 d 10 + 4)],
             ["attack 2x", "unarmed strike as bonus action", versatile(1 d 8)]),
      attack_variant(quarterstaff:twohanded, feet(5), to_hit(10),
                     [damage(bludgeoning, 1 d 8 + 4)],
                     []),
      ac(unarmored_defense(monk), 17, [])
    ]).
