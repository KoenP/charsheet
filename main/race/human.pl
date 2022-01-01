race_option(human).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
subrace(human, standard).
bonus_source(race(human(standard)), Ability+1) :-
    ability(Ability).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
subrace(human, variant).
bonus_options_source(race(human(variant)), asi, id, 2 unique_from ability_plus_n(1)).
trait_options_source(race(human(variant)), skill, wrap(skill), skill).
