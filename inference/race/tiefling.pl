race_option(tiefling).
racial_speed(tiefling, walking, 30).
race_shorthand(tiefling, tf).
bonus_source(race(tiefling), int+1).
bonus_source(race(tiefling), cha+2).

traits_from_source(race(tiefling), [darkvision(60),
                                    resistance(fire, half),
                                    trait('infernal legacy'),
                                    language(common),
                                    language(infernal)]).

known_spell(race(tiefling), cha, always, [], no, thaumaturgy) :-
    race(tiefling).
known_spell(race(tiefling), cha, always, [per_rest(1)], no, 'hellish rebuke') :-
    race(tiefling),
    match_level(3).
known_spell(race(tiefling), cha, always, [per_rest(1)], no, darkness) :-
    race(tiefling),
    match_level(5).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% DESCRIPTIONS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

'infernal legacy' ?= "You know the 'darkness' spell. When you reach 3rd level, you can cast the 'hellish rebuke' spell as a 2nd-level spell once per long rest. When you reach 5th level, you can also cast the 'darkness' spell once per long rest. Charisma is your spellcasting ability for these spells.".
