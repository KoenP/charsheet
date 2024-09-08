race_option(bugbear).
racial_speed(bugbear, walking, 30).
race_shorthand(bugbear, bb).
grant_racial_asis_plus2_plus1(bugbear).

% Languages
trait_source(race(bugbear), language(common)).
trait_options_source(race(bugbear), language, wrap(language), language).


traits_from_source(race(bugbear),
                   [ sense(darkvision),
                     'fey ancestry',
                     'long-limbed',
                     'powerful build',
                     sneaky,
                     'surprise attack'
                   ]).

trait_source(trait(sneaky), skill(stealth)).
bonus_source(trait('long-limbed'), range(Weapon) + feet(5)) :-
    weapon(Weapon, _, melee, _, _).
