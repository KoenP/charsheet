:- discontiguous cleric_domain_spell/2.

class_option(cleric).
hd_per_level(cleric, 1 d 8).
initial_class_base_hp(cleric, 8).
max_hp_per_level(cleric, 1 d 8).
caster(cleric, full).
spellcasting_ability(cleric, wis).
max_prepared_spells(cleric, N) :-
    default_max_prepared_spells(cleric, N).
choose_subclass_level(cleric:1).
asi_level(cleric:L) :-
    default_asi_level(L).
class_saving_throw(cleric, wis).
class_saving_throw(cleric, cha).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Spellcasting.

% Clerics get to pick cantrips.
known_spell(cleric, wis, always, [], no, Name) :-
    class_choice(cleric, cantrip, Name).
options_source(class(cleric), cantrip, 3 unique_from class_cantrip(cleric)).
options_source(match_class(cleric:L), cantrip, class_cantrip(cleric)) :-
    L=4; L=10.

% Clerics know all proper spells on their spell list.
% These always need to be prepared, with the exception of domain spells.
known_spell(cleric, wis, 'when prepared', [slot], Ritual, Name) :-
    learnable_proper_spell(cleric, Name),
    subclass(Class), Class =.. [cleric, Domain],
    \+ cleric_domain_spell(Domain, Name),
    spell_property(Name, ritual, Ritual).

% Domain spells are always prepared.
known_spell(cleric, wis, always, [slot], Ritual, Name) :-
    subclass(Class), Class =.. [cleric, Domain],
    cleric_domain_spell(Domain, Name),
    learnable_proper_spell(cleric, Name),
    spell_property(Name, ritual, Ritual).

% Add domain spells to the cleric spell list.
extend_class_spell_list(cleric, Spell) :-
    subclass(Class),
    Class =.. [cleric, Domain],
    cleric_domain_spell(Domain, Spell).
    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Initial class features.
traits_from_source(initial_class(cleric),
                   [armor(light), armor(medium), shield,
                    weapon(simple)]).

trait_options_source(initial_class(cleric), skill, wrap(skill),
                     2 unique_from from_list(
                         [history,insight,medicine,persuasion,religion])).

trait_source(class(cleric), spellcasting_focus(divine)).
trait_source(class(cleric), ritual_casting(cleric)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Level 2 features.
resource('channel divinity', 'channel divinity', N) :-
    class_level(cleric:Lvl),
    ordered_lookup_largest_leq([2 -> 1, 6 -> 2, 18 -> 3], Lvl, N).
trait_source(match_class(cleric:2), channel_divinity('turn undead')).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Level 5 features.
trait_source(match_class(cleric:5), destroy_undead(cr(CR))) :-
    class_level(cleric:L),
    ordered_lookup_largest_leq([5->1/2, 8->1, 11->2, 14->3, 17->4], L, CR).

meta_todo(cleric, "All features for >lvl 5 and all descriptions").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% DOMAINS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Knowledge domain.
subclass_option(cleric, knowledge).

cleric_domain_spell(knowledge, command).
cleric_domain_spell(knowledge, identify).
cleric_domain_spell(knowledge, augury).
cleric_domain_spell(knowledge, suggestion).
cleric_domain_spell(knowledge, nondetection).
cleric_domain_spell(knowledge, 'speak with dead').
cleric_domain_spell(knowledge, 'arcane eye').
cleric_domain_spell(knowledge, confusion).
cleric_domain_spell(knowledge, 'legend lore').
cleric_domain_spell(knowledge, scrying).

% Blessings of Knowledge
trait_source(match_class(cleric(knowledge)), 'blessings of knowledge').
trait_options(trait('blessings of knowledge'), language, wrap(skill),
              2 unique_from language) :-
    match_class(cleric(knowledge)).
trait_options(trait('blessings of knowledge'), skill, wrap(skill),
              2 unique_from from_list([arcana,history,nature,religion])) :-
    match_class(cleric(knowledge)).
trait(trait('blessings of knowledge'), expertise(skill(Skill))) :-
    choice_member(trait('blessings of knowledge'), skill, Skill).
meta_todo(nontermination, "why can't the blessings of knowledge trait options refer to the blessings of knowledge trait without causing an infinite loop? [later note: I don't know what this is about, I can't reproduce a nontermination issue here]").
meta_todo(trait('blessings of knowledge'), "Technically it's not expertise, but mechanically I'm not sure it's worth making a distinction here. In particular if it's not expertise, I'm not sure how/whether it stacks with expertise.").

% Knowledge of the ages.
trait_source(match_class(cleric(knowledge):2), 'knowledge of the ages').
trait_source(match_class(cleric(knowledge):2), channel_divinity('read thoughts')).
    
meta_todo(cleric(knowledge), "Complete this subclass implementation.").


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% DESCRIPTIONS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
