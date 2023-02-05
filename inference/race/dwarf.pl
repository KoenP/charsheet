race_option(dwarf).
racial_speed(dwarf, 25).
bonus_source(race(dwarf), con+2).
trait_source(race(dwarf), sense(darkvision)).
trait_source(race(dwarf), 'no heavy armor speed penalty').
trait_source(race(dwarf), 'dwarven resilience').
trait_source(trait('dwarven resilience'), resistance(poison)).
traits_from_source(race(dwarf),
                   [weapon(battleaxe), weapon(handaxe),
                    weapon('light hammer'), weapon(warhammer)]).
trait_options_source(race(dwarf), tool, wrap(tool),
                     from_list([smith, brewer, mason])).
trait_source(race(dwarf), stonecunning).
traits_from_source(race(dwarf), [language(common), language(dwarvish)]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
subrace_option(dwarf, 'hill dwarf').
bonus_source(race(dwarf('hill dwarf')), wis+1).
trait_source(race(dwarf('hill dwarf')), 'dwarven toughness').
bonus_source(trait('dwarven toughness'), 'max hp'+Lvl) :-
    level(Lvl).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
'dwarven toughness' ?= "Your hit point maximum increases by 1, and it increases by 1 every time you gain a level.".

'dwarven resilience' ?=
  "Advantage on saving throws against poison and resistance against poison damage".

stonecunning ?= "Whenever you make an Intelligence (History) check related to the origin of stonework, you are considered proficient in the History skill and add double your proficiency bonus to the check, instead of your normal proficiency bonus.".
