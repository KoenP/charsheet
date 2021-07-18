:- multifile
       base_race_option/1,
       race_size/2,
       race_base_speed/2,
       base_race/1,
       racial_trait/2,
       racial_trait_options/3,
       racial_spellcasting_ability/1,
       subrace/2.

:- [races/elf].
:- [races/human].
:- [races/tortle].

race_option(Race) :-
    base_race_option(Race) ; subrace(_, Race).

% Racial traits are those traits obtained by belonging to some race.
trait(race(Race), Trait) :-
    race(Race),
    racial_trait(Race, Trait).

trait_options(race(Race), Name, Options) :-
    race(Race),
    racial_trait_options(Race, Name, Options).

% Collect racial ability score increases.
racial_asi(Ability+Increment) :-
    trait(race(_), Ability+Increment).

% Handle subraces.
race(Race) :-
    subrace(Race, Subrace),
    race(Subrace).

most_specific_race(Race) :-
    race(Race),
    \+ subrace(Race, _).

% TODO for now assuming that subraces only go one level deep, perhaps
% I should extend this to support arbitrarily deep nesting.
problem(race([Race1,Race2]), multiple_races) :-
    user_picked_multiple_races(Race1, Race2).
user_picked_multiple_races(Race1, Race2) :-
    race(Race1),
    race(Race2),
    Race1 \= Race2,
    \+ (subrace(Race1,Race2) ; subrace(Race2,Race1)),
    !.

% Handle racial cantrips.
spell_known(Spell, Race, Ability, always_available, at_will) :-
    race(Race),
    trait(learn_spell(Race, Spell)),
    racial_spellcasting_ability(Ability),
    spell(Spell, level, 0).

% Specify which racial traits we wish to list in the general 'traits'
% section of the character sheet.
list_trait(Trait) :-
    trait(race(_), Trait),
    \+ member(Trait, [_+_, skill(_), language(_), weapon(_)]).
