test_char_level(
    evocwiz,
    1,
    [choice(init, 'initial class', wizard),
     choice(init, 'base race', dragonborn),
     base_ability(str, 11),
     base_ability(dex, 12),
     base_ability(con, 15),
     base_ability(wis, 10),
     base_ability(int, 18),
     base_ability(cha, 8),

     choice(wizard>:1, cantrip, ['fire bolt', 'acid splash', 'ray of frost']),

     asserted_has('enhanced arcane focus'), % boosts to hit and adds a note

     name(evocwiz)],
    [attack('fire bolt', feet(120), to_hit(7), [damage(fire, 1 d 10)], ["ignore half cover"]),
     attack('ray of frost', feet(60), to_hit(7), [damage(cold, 1 d 8)], ["ignore half cover"]),
     attack('acid splash', feet(60), saving_throw(14,dex), [damage(acid, 1 d 6)], ["up to two targets within 5 ft of eachother"])
    ]).

test_char_level(
    evocwiz,
    2,
    [choice(level(2), 'as class', wizard),
     choice(level(2), 'max hp roll'(_,_), 5),
     choice(wizard>:2, subclass, evocation)],
    []).

test_char_level(evocwiz, L, [choice(level(L), 'as class', wizard),
     choice(level(L), 'max hp roll'(_,_), 5)], []) :-
    between(3, 5, L).

test_char_level(
    evocwiz,
    6,
    [choice(level(6), 'as class', wizard),
     choice(level(6), 'max hp roll'(_,_), 5)],
    [attack('fire bolt', feet(120), to_hit(8), [damage(fire, 2 d 10)], ["ignore half cover"]),
     attack('ray of frost', feet(60), to_hit(8), [damage(cold, 2 d 8)], ["ignore half cover"]),
     attack('acid splash', feet(60), saving_throw(15,dex), [damage(acid, 2 d 6)], ["up to two targets within 5 ft of eachother", "on save: half damage"])
    ]).
