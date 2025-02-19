test_char_level(
    extra_attack,
    1,
    [choice(init, 'base race', 'human'),
     choice(init, 'initial class', monk),
     base_ability(str,14),
     base_ability(dex,18),
     base_ability(con,12),
     base_ability(int,10),
     base_ability(wis,16),
     base_ability(cha,8),
     name(extra_attack)],
    []).

% Get to level 5 monk, checkthat we get the extra attack feature.
test_char_level(
    extra_attack,
    L,
    [choice(level(L), 'as class', monk),
     choice(level(L), 'max hp roll'(_,_), 5)], []) :-
    between(2, 4, L).
test_char_level(
    extra_attack,
    5,
    [choice(level(5), 'as class', monk),
     choice(level(5), 'max hp roll'(_,_), 5)],
    [trait(monk >: 5, extra_attack(1))]).

% Add level 5 fighter multiclass, verify that the extra attacks don't stack.
test_char_level(
    extra_attack,
    L,
    [choice(level(L), 'as class', fighter),
     choice(level(L), 'max hp roll'(_,_), 5)], []) :-
    between(6, 9, L).
test_char_level(
    extra_attack,
    10,
    [choice(level(10), 'as class', fighter),
     choice(level(10), 'max hp roll'(_,_), 5)],
    [findall(Origin, trait(Origin, extra_attack(1)), [_]) % Make sure there's only one,
                                                          % doesn't matter which.
    ]).

% Add 6 levels of fighter to go to level 11 fighter (gain 2 extra attacks)
test_char_level(
    extra_attack,
    L,
    [choice(level(L), 'as class', fighter),
     choice(level(L), 'max hp roll'(_,_), 5)], []) :-
    between(11, 15, L).
test_char_level(
    extra_attack,
    16,
    [choice(level(16), 'as class', fighter),
     choice(level(16), 'max hp roll'(_,_), 5)],
    % This time we need to make sure that it's specifically the fighter trait that remains.
    [findall(Origin-N,
             trait(Origin, extra_attack(N)),
             [(fighter >: _) - 2])
    ]).
