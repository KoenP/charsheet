race_option(elf).
racial_speed(elf, walking, 30).
race_shorthand(elf, el).

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

spellcasting_ability('high elf', int).
known_spell('high elf', int, always, [], no, Cantrip) :-
    choice(race(elf('high elf')), cantrip, Cantrip).
spell_origin_shorthand('high elf', elf).

trait_options_source(race(elf('high elf')), language, wrap(language), language).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% DESCRIPTIONS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

'fey ancestry' ?= "You have advantage on saving throws against being charmed, and magic can’t put you to sleep.".
'keen senses' ?= "You have proficiency in the Perception skill.".
'trance' ?= "Elves don’t need to sleep. Instead, they meditate deeply, remaining semiconscious, for 4 hours a day. (The Common word for such meditation is \"trance.\") While meditating, you can dream after a fashion; such dreams are actually mental exercises that have become reflexive through years of practice. After resting in this way, you gain the same benefit that a human does from 8 hours of sleep.".

'fey ancestry' @= srd('23').
'keen senses' @= srd('23').
trance @= srd('23').
