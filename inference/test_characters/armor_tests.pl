test_char_level(
    monkbarb,
    1,
    [name(monkbarb),
     base_ability(wis,20),
     base_ability(con,18),
     base_ability(dex,16),

     base_ability(str,10),
     base_ability(int,10),
     base_ability(cha,10),

     asserted_has(shield + 1),

     choice(init, 'initial class', monk)
    ],
    [ac_formula(unarmored_defense(monk), 10 + dex + wis),
     ac(unarmored_defense(monk), 18, [])
    ]).

test_char_level(
    monkbarb,
    2,
    [choice(level(2), 'as class', barbarian),
choice(level(2), 'max hp roll'(_,_), 8)],
    [ac_formula(unarmored_defense(monk), 10 + dex + wis),
     %ac_formula(unarmored_defense(barbarian), 10 + dex + con + shield),
     ac(unarmored_defense(monk), 18, [])
     %ac(unarmored_defense(barbarian), 20) % 10 + 3 + 4 + 3
    ]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

test_char_level(
    barbmonk,
    1,
    [name(barbmonk),
     base_ability(wis,20),
     base_ability(con,18),
     base_ability(dex,16),

     base_ability(str,10),
     base_ability(int,10),
     base_ability(cha,10),

     asserted_has(shield + 1),

     choice(init, 'initial class', barbarian)
    ],
    []).
test_char_level(
    barbmonk,
    2,
    [choice(level(2), 'as class', monk),
choice(level(2), 'max hp roll'(_,_), 8)],
    [ac_formula(unarmored_defense(barbarian), 10 + dex + con + shield),
     ac(unarmored_defense(barbarian), 17, [shield(shield+1) : 3]) % 10 + 3 + 4
    ]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

test_char_level(
    dracsorcbarb,
    1,
    [name(dracsorcbarb),
     base_ability(wis,20),
     base_ability(con,18),
     base_ability(dex,16),

     base_ability(str,10),
     base_ability(int,10),
     base_ability(cha,10),

     asserted_has(shield + 1),

     choice(init, 'initial class', sorcerer),
     choice(sorcerer >: 1, subclass, 'draconic bloodline')
    ],
    [ac_formula(trait('draconic resilience'), 13 + dex + shield),
     ac(trait('draconic resilience'), 16, []) % no shield proficiency
    ]).
test_char_level(
    dracsorcbarb,
    2,
    [choice(level(2), 'as class', barbarian),
     choice(level(2), 'max hp roll'(_,_), 8)],
    [ac_formula(trait('draconic resilience'), 13 + dex + shield),
     ac(trait('draconic resilience'), 16, [shield(shield+1) : 3]), % with shield proficiency
     ac_formula(unarmored_defense(barbarian), 10 + dex + con + shield),
     ac(unarmored_defense(barbarian), 17, [shield(shield+1) : 3])
    ]).
