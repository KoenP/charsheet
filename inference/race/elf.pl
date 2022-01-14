race_option(elf).
race_shorthand(el).

subrace(elf, 'high elf').
traits_from_source(race(elf), [sense(darkvision), 
                               'fey ancestry',
                               sense('keen senses'),
                               trance,
                               language(common),
                               language(elvish)]).
trait_source(trait(sense('keen senses')), skill(perception)).
bonus_source(race(elf), dex+2).

bonus_source(race('high elf'), int+1).
traits_from_source(race('high elf'), [weapon(longsword),
                                      weapon(shortsword),
                                      weapon(shortbow),
                                      weapon(longbow)]).

meta_todo(race('high elf'), learnable_cantrip(wizard)).
meta_todo(race('high elf'), language).
%options_source(race('high elf'), cantrip, learnable_cantrip(wizard)).
%options_source(race('high elf'), language, language).
