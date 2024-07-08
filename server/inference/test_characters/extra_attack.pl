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
test_char_level(extra_attack, L, [gain_level(L, monk, hp_avg)], []) :-
    between(2, 4, L).
test_char_level(
    extra_attack,
    5,
    [gain_level(5, monk, hp_avg)],
    [trait(monk >: 5, extra_attack(1))]).

% Add level 5 fighter multiclass, verify that the extra attacks don't stack.
test_char_level(extra_attack, L, [gain_level(L, fighter, hp_avg)], []) :-
    between(6, 9, L).
test_char_level(
    extra_attack,
    10,
    [gain_level(10, fighter, hp_avg)],
    [findall(Origin, trait(Origin, extra_attack(1)), [_]) % Make sure there's only one,
                                                          % doesn't matter which.
    ]).

% Add 6 levels of fighter to go to level 11 fighter (gain 2 extra attacks)
test_char_level(extra_attack, L, [gain_level(L, fighter, hp_avg)], []) :-
    between(11, 15, L).
test_char_level(
    extra_attack,
    16,
    [gain_level(16, fighter, hp_avg)],
    % This time we need to make sure that it's specifically the fighter trait that remains.
    [findall(Origin-N,
             trait(Origin, extra_attack(N)),
             [(fighter >: _) - 2]) 
    ]).
