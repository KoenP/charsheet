race_option(tiefling).
racial_speed(tiefling, walking, 30).
race_shorthand(tiefling, tf).
bonus_source(race(tiefling), int+1).
bonus_source(race(tiefling), cha+2).

traits_from_source(race(tiefling), [darkvision(60),
                                    resistance(fire, half),
                                    'infernal legacy',
                                    language(common),
                                    language(infernal)]).

known_spell(tiefling, cha, always, [], no, thaumaturgy) :-
    race(tiefling).
known_spell(tiefling, cha, always, [per_rest(long,1)], no, 'hellish rebuke') :-
    race(tiefling),
    match_level(3).
known_spell(tiefling, cha, always, [per_rest(long,1)], no, darkness) :-
    race(tiefling),
    match_level(5).
spellcasting_ability(tiefling, cha).
spell_origin_shorthand(tiefling, tfl).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% DESCRIPTIONS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

'infernal legacy' ?= "You know the thaumaturgy cantrip. When you reach 3rd level, you can cast the hellish rebuke spell as a 2nd-level spell once with this trait and regain the ability to do so when you finish a long rest. When you reach 5th level, you can cast the darkness spell once with this trait and regain the ability to do so when you finish a long rest. Charisma is your spellcasting ability for these spells.".

'infernal legacy'@=srd('43').
