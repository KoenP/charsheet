race_option(half_orc).
racial_speed(half_orc, 30).
bonus_source(race(half_orc), str+2).
bonus_source(race(half_orc), con+1).
trait_source(race(half_orc), sense(darkvision)).
trait_source(race(half_orc), menacing).
trait_source(race(half_orc), 'savage attacks').
traits_from_source(race(half_orc),
                   [language(common), language(orcish)]).

trait_source(race(half_orc), 'relentless endurance').
resource('relentless endurance', 'relentless endurance', 1) :-
    trait('relentless endurance').
on_rest(long, 'relentless endurance', full_restore).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
'menacing' ?= "You gain proficiency in the Intimidation skill.".

'relentless endurance' ?=
  "When you are reduced to 0 hit points but not killed outright, you can drop to 1 hit point instead. You can't use this feature again until you finish a long rest.".

'savage attacks' ?= "When you score a critical hit with a melee weapon attack, you can roll one of the weapon's damage dice one additional time and add it to the extra damage of the critical hit.".
