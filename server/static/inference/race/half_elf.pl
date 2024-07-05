race_option('half-elf').
racial_speed('half-elf', 30).
race_shorthand(he).

bonus_source(race('half-elf'), cha+2).
bonus_options_source(race('half-elf'), asi, id, 2 from ability_plus_n(1)).
traits_from_source(race('half-elf'), ['fey ancestry',
                                      sense(darkvision),
                                      language(common),
                                      language(elvish)]).
trait_options_source(race('half-elf'), 'skill versatility', wrap(skill),
                     2 unique_from skill).
trait_options_source(race('half-elf'), language, wrap(language), language).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% DESCRIPTIONS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

'fey ancestry' ?= "You have advantage on saving throws against being charmed, and magic can’t put you to sleep.".
