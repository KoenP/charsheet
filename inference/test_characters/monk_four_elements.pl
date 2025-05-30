test_char_level(
    fourelements,
    1,
    [choice(init, 'initial class', monk),

     base_ability(str,14),
     base_ability(dex,18),
     base_ability(con,12),
     base_ability(int,10),
     base_ability(wis,16),
     base_ability(cha,8),

     name(fourelements)],
    []
).

test_char_levelup_fromto(fourelements, monk, 2-2).

test_char_level(
    fourelements,
    3,
    [choice(level(3), 'as class', monk),
     choice(level(3), 'max hp roll'(_,_), 5),
     choice(monk >: 3, subclass, 'four elements'),
     choice(monk('four elements') >: 3, 'elemental discipline', 'fist of four thunders')
    ],
    [trait('elemental discipline'('elemental attunement')),
     trait('elemental discipline'('fist of four thunders')),
     hide_base_option(monk('four elements') >: 3, 'elemental discipline', 'elemental attunement')
    ]
).

test_char_levelup_fromto(fourelements, monk, 4-10).

test_char_level(
    fourelements,
    11,
    [choice(level(11), 'as class', monk),
     choice(level(11), 'max hp roll'(_,_), 5),
     choice(monk('four elements') >: 11, 'elemental discipline', 'mist stance'),
     choice(monk('four elements') >: 11, replace('elemental discipline'), 'elemental attunement'),
     choice(monk('four elements') >: 11,
            replacing('elemental discipline', 'elemental attunement'),
            'fangs of the fire snake')
    ],
    [hide_base_option(monk('four elements') >: 11, 'elemental discipline', 'fist of four thunders'),
     hide_base_option(monk('four elements') >: 11, replacing('elemental discipline',_), 'fist of four thunders'),
     \+hide_base_option(monk('four elements') >: 11, 'elemental discipline', 'elemental attunement'),

     known_spell_property(_, 'gaseous form', range, self),
     known_spell_property(monk('elemental discipline'('mist stance')),
                          'gaseous form',
                          components,
                          [v,s]), % material component removed
     trait('elemental discipline'('fangs of the fire snake'))
    ]
).
