race_option(halfling).
race_shorthand(halfling, hf).
racial_speed(halfling, 25).
bonus_source(race(halfling), dex+2).
trait_source(race(halfling), lucky).
trait_source(race(halfling), brave).
trait_source(race(halfling), 'halfling nimbleness').
traits_from_source(race(halfling), [language(common), language(halfling)]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

subrace_option(halfling, lightfoot).
bonus_source(race(halfling), cha+2).
trait_source(race(halfling(lightfoot)), 'naturally stealthy').

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

lucky ?= "When you roll a 1 on the d20 for an attack roll, ability check, or saving throw, you can reroll the die and must use the new roll.".

brave ?= "You have advantage on saving throws against being frightened.".

'halfling nimbleness' ?= "You can move through the space of any creature that is of a size larger than yours.".

'naturally stealthy' ?= "You can attempt to hide even when you are obscured only by a creature that is at least one size larger than you.".
