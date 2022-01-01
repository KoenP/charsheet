base_race_option(human).
race_size(human, medium).
race_base_speed(human, 30).
racial_trait(human, language(common)).
racial_trait_options(human, language, 1 from [language(_)]).
racial_trait(human, Ability + 1) :-
    ability(Ability).
