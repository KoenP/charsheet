test_char_level(
    mmadept,
    1,
    [name(mmadept),
     base_ability(str,10),
     base_ability(dex,10),
     base_ability(con,10),
     base_ability(int,10),
     base_ability(wis,10),
     base_ability(cha,10),

     choice(init, 'base race', human),
     choice(race(human), subrace, standard),

     choice(init, 'initial class', wizard)],
    []
).

test_char_level(mmadept, L, [choice(level(L), 'as class', wizard),
     choice(level(L), 'max hp roll'(_,_), 5)], []) :-
    between(2, 3, L).

test_char_level(
    mmadept,
    4,

    [choice(level(4), 'as class', wizard),
     choice(level(4), 'max hp roll'(_,_), 5),
     choice(wizard >: 4, 'asi or feat', 'metamagic adept'),
     choice(feat('metamagic adept'), metamagic, ['careful spell', 'distant spell'])],

    [\+ options(feat('metamagic adept') at 4, replace(metamagic), _),
     selected_at_character_level(feat('metamagic adept'), 4, metamagic, 'careful spell'),
     selected_at_character_level(feat('metamagic adept'), 4, metamagic, 'distant spell'),
     trait(feat('metamagic adept'), metamagic('careful spell')),
     trait(feat('metamagic adept'), metamagic('distant spell')),
     res('metamagic adept sorcery point', 2)]
).

test_char_level(mmadept, L, [choice(level(L), 'as class', wizard),
     choice(level(L), 'max hp roll'(_,_), 5)], []) :-
    between(5, 7, L).

test_char_level(
    mmadept,
    8,

    [choice(level(8), 'as class', wizard),
     choice(level(8), 'max hp roll'(_,_), 5),
     choice(feat('metamagic adept') at 8, replace(metamagic), 'careful spell'),
     choice(feat('metamagic adept') at 8, replacing(metamagic, 'careful spell'), 'empowered spell')
    ],

    [\+ trait(feat('metamagic adept'), metamagic('careful spell')),
     trait(feat('metamagic adept'), metamagic('empowered spell')),
     trait(feat('metamagic adept'), metamagic('distant spell'))]
).

test_char_level(mmadept, L, [choice(level(L), 'as class', wizard),
     choice(level(L), 'max hp roll'(_,_), 5)], []) :-
    between(9, 11, L).

% Check that reintroducing a choice that was replaced earlier works.
test_char_level(
    mmadept,
    12,

    [choice(level(12), 'as class', wizard),
     choice(level(12), 'max hp roll'(_,_), 5),
     choice(feat('metamagic adept') at 12, replace(metamagic), 'distant spell'),
     choice(feat('metamagic adept') at 12, replacing(metamagic, 'distant spell'), 'careful spell')
    ],

    [\+ trait(feat('metamagic adept'), metamagic('distant spell')),
     trait(feat('metamagic adept'), metamagic('empowered spell')),
     trait(feat('metamagic adept'), metamagic('careful spell'))]
).
