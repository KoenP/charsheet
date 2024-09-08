race_option(firbolg).
racial_speed(firbolg, walking, 30).
race_shorthand(firbolg, fb).
grant_racial_asis_plus2_plus1(firbolg).

% Languages.
trait_source(race(bugbear), language(common)).
trait_options_source(race(bugbear), language, wrap(language), language).

% Firbolg magic.
known_spell(firbolg, Abi, always, ['firbolg spell slot' or slot], no, Spell) :-
    race(firbolg),
    spellcasting_ability(firbolg, Abi),
    (Spell = 'detect magic' ; Spell = 'disguise self').
res('firbolg spell slot', 1) :-
    race(firbolg).
on_rest(long, 'firbolg spell slot', 'full restore').
bonus_source(race(firbolg), modify_spell(firbolg, 'disguise self', Goal)) :-
    Goal = add_spell_effects(["You can appear up to 3 ft shorter or taller"]).

trait_options_source(race(firbolg), 'firbolg spellcasting ability', wrap(firbolg_spellcasting_ability),
                     from_list([int, wis, cha])).
spellcasting_ability(firbolg, Abi) :-
    trait(firbolg_spellcasting_ability(Abi)).
    
% TODO
% - modify disguise self
% - firbolg spell slot resource
% - make sure that "or" works
% - put this info on the card somehow
    
