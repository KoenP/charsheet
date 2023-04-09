:- dynamic trait/1.

test_char_level(
    monk,
    1,
    [choice(init, 'base race', 'half orc'),
     choice(init, 'initial class', monk),
     has(club),
     has(quarterstaff),

     base_ability(str,14),
     base_ability(dex,18),
     base_ability(con,12),
     base_ability(int,10),
     base_ability(wis,16),
     base_ability(cha,8),

     name(monk)],
    [speed(30),
     attack(club, feet(5), to_hit(6), [damage(bludgeoning, 1 d 4 + 4)],
            [light, "+ unarmed strike as bonus action"]),
     attack(unarmed, feet(5), to_hit(6), [damage(bludgeoning, 1 d 4 + 4)],
            ["+ unarmed strike as bonus action"]),
     attack(quarterstaff, feet(5), to_hit(6), [damage(bludgeoning, 1 d 6 + 4)],
            [versatile(1 d 8), "+ unarmed strike as bonus action"]),
     attack_variant(quarterstaff:twohanded, feet(5), to_hit(6), [damage(bludgeoning, 1 d 8 + 4)],
            []),
     ac(unarmored, 14, []),
     ac(unarmored_defense(monk), 17, [])]
).

test_char_level(
    monk,
    2,
    [gain_level(2, fighter, hp_avg), % a level of fighter to get shield proficiency
     has(shield) % shouldn't show up in unarmored defense
    ],
    [attack(club, feet(5), to_hit(6), [damage(bludgeoning, 1 d 4 + 4)],
            [light, "+ unarmed strike as bonus action"]),
     attack(unarmed, feet(5), to_hit(6), [damage(bludgeoning, 1 d 4 + 4)],
            ["+ unarmed strike as bonus action"]),
     attack(quarterstaff, feet(5), to_hit(6), [damage(bludgeoning, 1 d 6 + 4)],
            [versatile(1 d 8), "+ unarmed strike as bonus action"]),
     attack_variant(quarterstaff:twohanded, feet(5), to_hit(6), [damage(bludgeoning, 1 d 8 + 4)],
            []),
     ac(unarmored, 14, [shield:2]),
     ac(unarmored_defense(monk), 17, [])]
).

test_char_level(
    monk,
    3,
    [gain_level(3, monk, hp_avg) % a level of fighter to get shield proficiency
    ],
    [resource(ki, 'ki points', 2),
     speed(40),
     attack(club, feet(5), to_hit(6), [damage(bludgeoning, 1 d 4 + 4)],
            [light, "+ unarmed strike as bonus action"]),
     attack(unarmed, feet(5), to_hit(6), [damage(bludgeoning, 1 d 4 + 4)],
            ["+ unarmed strike as bonus action"]),
     attack(quarterstaff, feet(5), to_hit(6), [damage(bludgeoning, 1 d 6 + 4)],
            [versatile(1 d 8), "+ unarmed strike as bonus action"]),
     attack_variant(quarterstaff:twohanded, feet(5), to_hit(6), [damage(bludgeoning, 1 d 8 + 4)],
            []),
     ac(unarmored, 14, [shield:2]),
     ac(unarmored_defense(monk), 17, [])]
).

test_char_level(
    monk,
    4,
    [gain_level(4, monk, hp_avg)],
    [resource(ki, 'ki points', 3),
     speed(40),
     attack(club, feet(5), to_hit(6), [damage(bludgeoning, 1 d 4 + 4)],
            [light, "+ unarmed strike as bonus action"]),
     attack(unarmed, feet(5), to_hit(6), [damage(bludgeoning, 1 d 4 + 4)],
            ["+ unarmed strike as bonus action"]),
     attack(quarterstaff, feet(5), to_hit(6), [damage(bludgeoning, 1 d 6 + 4)],
            [versatile(1 d 8), "+ unarmed strike as bonus action"]),
     attack_variant(quarterstaff:twohanded, feet(5), to_hit(6), [damage(bludgeoning, 1 d 8 + 4)],
            []),
     ac(unarmored, 14, [shield:2]),
     ac(unarmored_defense(monk), 17, [])]
).

test_char_level(monk, 5, [gain_level(5, monk, hp_avg)], []).

test_char_level(
    monk, 6, [gain_level(6, monk, hp_avg)],
    [ gain_level(6, monk, hp_avg),
      resource(ki, 'ki points', 5),
      speed(40),
      attack(club, feet(5), to_hit(7), [damage(bludgeoning, 1 d 6 + 4)],
             [light, "+ unarmed strike as bonus action"]),
      attack(unarmed, feet(5), to_hit(7), [damage(bludgeoning, 1 d 6 + 4)],
             ["+ unarmed strike as bonus action"]),
      attack(quarterstaff, feet(5), to_hit(7), [damage(bludgeoning, 1 d 6 + 4)],
             [versatile(1 d 8), "+ unarmed strike as bonus action"]),
      attack_variant(quarterstaff:twohanded, feet(5), to_hit(7),
                     [damage(bludgeoning, 1 d 8 + 4)],
                     []),
      ac(unarmored, 14, [shield:2]),
      ac(unarmored_defense(monk), 17, [])
    ]).

test_char_level(monk, L, [gain_level(L, monk, hp_avg)], []) :-
    between(7,17,L).

% monk level 17
test_char_level(
    monk, 18, [gain_level(18, monk, hp_avg)],
    [ gain_level(18, monk, hp_avg),
      resource(ki, 'ki points', 17),
      speed(55),
      attack(club, feet(5), to_hit(10), [damage(bludgeoning, 1 d 10 + 4)],
             [light, "+ unarmed strike as bonus action"]),
      attack(unarmed, feet(5), to_hit(10), [damage(bludgeoning, 1 d 10 + 4)],
             ["+ unarmed strike as bonus action", magical]),
      attack(quarterstaff, feet(5), to_hit(10), [damage(bludgeoning, 1 d 10 + 4)],
             [versatile(1 d 8), "+ unarmed strike as bonus action"]),
      attack_variant(quarterstaff:twohanded, feet(5), to_hit(10),
                     [damage(bludgeoning, 1 d 8 + 4)],
                     []),
      ac(unarmored, 14, [shield:2]),
      ac(unarmored_defense(monk), 17, [])
    ]).
