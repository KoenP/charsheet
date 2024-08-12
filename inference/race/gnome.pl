race_option(gnome).
race_shorthand(gnome, gn).
racial_speed(gnome, walking, 25).
bonus_source(race(gnome), int+2).
trait_source(race(gnome), sense(darkvision)).
trait_source(race(gnome), 'gnome cunning').
traits_from_source(race(gnome),
                   [language(common), language(gnomish)]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
subrace_option(gnome, 'rock gnome').
bonus_source(race(gnome('rock gnome')), con+1).
trait_source(race(gnome('rock gnome')), 'artificer\'s lore').
trait_source(race(gnome('rock gnome')), 'tinker').
trait_options_source(race(gnome('rock gnome')), tool, wrap(tool),
                     from_list([smith, brewer, mason, alchemist])).
traits_from_source(race(gnome('rock gnome')),
                   [language(common), language(gnomish)]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
'gnome cunning' ?= "You have advantage on all Intelligence, Wisdom, and Charisma saving throws against magic.".

'artificer\'s lore' ?=
  "Whenever you make an Intelligence (History) check related to magic items, alchemical objects, or technological devices, you can add twice your proficiency bonus, instead of any proficiency bonus you normally apply.".

'tinker' ?=
  "You have proficiency with artisan's tools (tinker's tools). Using those tools, you can spend 1 hour and 10 gp worth of materials to construct a Tiny clockwork device (AC 5, 1 hp). The device ceases to function after 24 hours (unless you spend 1 hour repairing it to keep the device functioning), or when you use your action to dismantle it; at that time, you can reclaim the materials used to create it. You can have up to three such devices active at a time.".
