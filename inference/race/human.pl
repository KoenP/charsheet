race_option(human).
racial_speed(human, 30).
race_shorthand(human, hu).

trait_source(race(human), language(common)).
trait_options_source(race(human), language, wrap(language), language).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

subrace_option(human, standard).
bonus_source(race(human(standard)), Ability+1) :-
    ability(Ability).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

subrace_option(human, variant).
bonus_options_source(race(human(variant)), asi, id, 2 unique_from ability).
trait_options_source(race(human(variant)), skill, wrap(skill), skill).
trait_options_source(race(human(variant)), feat, wrap(feat), selectable_feat_option).
