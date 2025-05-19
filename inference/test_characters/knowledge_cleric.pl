test_char_level(
    'knowledge cleric',
    1,
    [name('knowledge cleric'),

     base_ability(str,13),
     base_ability(dex,10),
     base_ability(con,14),
     base_ability(int,12),
     base_ability(wis,16),
     base_ability(cha,9),

     choice(init, 'initial class', cleric),
     choice(cleric >: 1, subclass, knowledge),

     choice(cleric >: 1, cantrip, ['sacred flame'])
    ],
    [known_spell_property(cleric, 'sacred flame', effects, [saving_throw(dex):damage(radiant, 1 d 8)])]
).

test_char_level('knowledge cleric', L,
                [choice(level(L), 'as class', cleric),
                 choice(level(L), 'max hp roll'(_,_), 5)],
                []) :-
    between(2, 7, L).

test_char_level(
    'knowledge cleric',
    8,
    [choice(level(8), 'as class', cleric),
     choice(level(8), 'max hp roll'(_,_), 5)
    ],
    [known_spell_property(cleric, 'sacred flame', effects, [saving_throw(dex):damage(radiant, 2 d 8 + 3)])]
).

