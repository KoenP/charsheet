base_race_option(elf).

race_size(elf, medium).
race_base_speed(elf, 30).
racial_spellcasting_ability(int). % Ability to use for the racial cantrip.

racial_trait(elf, dex+2).
racial_trait(elf, sense(darkvision(60))).
racial_trait(elf, keen_senses).
trait(keen_senses, skill(perception)) :- trait(keen_senses).
racial_trait(elf, fey_ancestry).


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

    
