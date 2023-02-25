race_option(elf).
racial_speed(elf, 30).
race_shorthand(el).

traits_from_source(race(elf), [sense(darkvision), 
                               'fey ancestry',
                               sense('keen senses'),
                               trance,
                               language(common),
                               language(elvish)]).
trait_source(trait(sense('keen senses')), skill(perception)).
bonus_source(race(elf), dex+2).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Subrace: high elf
subrace_option(elf, 'high elf').
bonus_source(race(elf('high elf')), int+1).
traits_from_source(race(elf('high elf')), [weapon(longsword),
                                           weapon(shortsword),
                                           weapon(shortbow),
                                           weapon(longbow)]).

options_source(race(elf('high elf')), cantrip, class_cantrip(wizard)).
known_spell(race(elf('high elf')), int, always, [], no, Cantrip) :-
    choice(race(elf('high elf')), cantrip, Cantrip).

trait_options_source(race('high elf'), language, wrap(language), language).
