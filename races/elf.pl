base_race_option(elf).

race_size(elf, medium).
race_base_speed(elf, 30).
racial_spellcasting_ability(int). % Ability to use for the racial cantrip.

racial_trait(elf, dex+2).
racial_trait(elf, sense(darkvision(60))).
racial_trait(elf, 'keen senses').
trait('keen senses', skill(perception)) :- trait('keen senses').
racial_trait(elf, 'fey ancestry').
'fey ancestry' ?= "You have advantage on saving throws against being charmed, and magic can’t put you to sleep.".
racial_trait(elf, trance).
trance ?= "Elves don’t need to sleep. Instead, they meditate deeply, remaining semiconscious, for 4 hours a day. (The Common word for such meditation is 'trance'.) While meditating, you can dream after a fashion; such dreams are actually mental exercises that have become reflexive through years of practice. After resting in this way, you gain the same benefit that a human does from 8 hours of sleep.".

subrace(elf, 'high elf').
subrace(elf, 'wood elf').

racial_trait('high elf', int+1).
racial_trait('high elf', weapon(longsword)).
racial_trait('high elf', weapon(shortsword)).
racial_trait('high elf', weapon(shortbow)).
racial_trait('high elf', weapon(longbow)).

racial_trait_options('high elf', cantrip, 1 from Cantrips) :-
    findall(learn_spell('high elf', Cantrip),
            (spell_class(Cantrip, wizard), spell(Cantrip, level, 0)),
            Cantrips).

    
