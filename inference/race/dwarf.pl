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
bonus_source(race(dwarf('hill dwarf')), 'max hp'+Lvl) :-
    level(Lvl).

meta_todo(dwarf('mountain dwarf'), "implement this subrace").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
'dwarven resilience' ?=
  "Advantage on saving throws against poison and resistance against poison damage".
