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
extend_class_spell_list(cleric, Spell) :-
    subclass(cleric(knowledge)),
    member(Spell, [command, identify, augury, suggestion,
                   nondetection, 'speak with dead', 'arcane eye', confusion,
                   'legend lore', scrying]).
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
meta_todo(nontermination, "why can't the blessings of knowledge trait options refer to the blessings of knowledge trait without causing an infinite loop?").
meta_todo(trait('blessings of knowledge'), "Technically it's not expertise, but mechanically I'm not sure it's worth making a distinction here. In particular if it's not expertise, I'm not sure how/whether it stacks with expertise.").

% Knowledge of the ages.
trait_source(match_class(cleric(knowledge):2), 'knowledge of the ages').
trait_source(match_class(cleric(knowledge):2), channel_divinity('read thoughts')).
    
meta_todo(cleric(knowledge), "Complete this subclass implementation.").


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% DESCRIPTIONS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
