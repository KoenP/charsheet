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
options_source(cleric >: 1, cantrip, 3 unique_from class_cantrip(cleric)).
options_source(cleric >: L, cantrip, class_cantrip(cleric)) :-
    L=4; L=10.

% Clerics know all proper spells on their spell list.
% These always need to be prepared, with the exception of domain spells.
known_spell(cleric, wis, 'when prepared', [slot], Ritual, Name) :-
    learnable_proper_spell(cleric, Name),
    \+ (subclass(Class), Class =.. [cleric, Domain], cleric_domain_spell(Domain, Name)),
    spell_property(Name, ritual, Ritual).

% Domain spells are always prepared.
known_spell(cleric(Domain), wis, always, [slot], Ritual, Name) :-
    subclass(cleric(Domain)),
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
traits_from_source(^cleric,
                   [armor(light), armor(medium), armor(shield),
                    weapon(simple)]).

trait_options_source(^cleric, skill, wrap(skill),
                     2 unique_from from_list(
                         [history,insight,medicine,persuasion,religion])).

trait_source(cleric >: 1, spellcasting_focus(divine)).
trait_source(cleric >: 1, ritual_casting(cleric)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Features from leveling up.
trait_source(cleric >: 2, 'channel divinity (cleric)').
trait_source(cleric >: 2, channel_divinity('turn undead')).
bonus_source(cleric >: 2, channel_divinity_uses(N)) :-
    class_level(cleric:Lvl),
    ordered_lookup_largest_leq([2 -> 1, 6 -> 2, 18 -> 3], Lvl, N).
meta_todo(cleric, "'channel divinity' has specific multiclassing rules").

trait_source(cleric >: 5, destroy_undead(cr(CR))) :-
    class_level(cleric:L),
    ordered_lookup_largest_leq([5->1/2, 8->1, 11->2, 14->3, 17->4], L, CR).

trait_source(cleric >: 10, divine_intervention(Pct pct)) :-
    class_level(cleric:Level),
    (Level < 20 -> Pct = Level ; Pct = 100).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% DOMAINS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Life domain.
% ------------
subclass_option(cleric, life).

cleric_domain_spell(life, bless).
cleric_domain_spell(life, 'cure wounds').
cleric_domain_spell(life, 'lesser restoration').
cleric_domain_spell(life, 'spiritual weapon').
cleric_domain_spell(life, 'beacon of hope').
cleric_domain_spell(life, revivify).
cleric_domain_spell(life, 'death ward').
cleric_domain_spell(life, 'guardian of faith').
cleric_domain_spell(life, 'mass cure wounds').
cleric_domain_spell(life, 'raise dead').

trait_source(cleric(life) >: 1, armor(heavy)).
trait_source(cleric(life) >: 1, 'disciple of life').
bonus_source(trait('disciple of life'),
             modify_spell(_, HealingSpell, increase_all_spell_healing_rolls(Bonus))) :-
    spell_property(HealingSpell, level, Level),
    Level >= 1,
    healing_spell(HealingSpell),
    Bonus is 2 + Level.

meta_todo(trait('disciple of life'), "How to communicate the upcasting effect on the spell card?").

trait_source(cleric(life) >: 2, channel_divinity(preserve_life(Pool))) :-
    class_level(cleric:L),
    Pool is 5*L.
custom_format(preserve_life(Pool)) --> ["Preserve life ("], [Pool], [" HP)"].
trait_source(cleric(life) >: 6, 'blessed healer').
bonus_source(trait('blessed healer'),
             modify_spell(_, HealingSpell, add_spell_effects([(target=other)->self_heal(HP)]))) :-
    spell_property(HealingSpell, level, Level),
    Level >= 1,
    \+ spell_property(HealingSpell, range, self),
    healing_spell(HealingSpell),
    HP is 2 + Level.
custom_format(self_heal(HP)) --> [" heal self for "], format_number(HP).

trait_source(cleric(life) >: 8, divine_strike(N d 8)) :-
    class_level(cleric:L),
    (L < 14 -> N = 1; N = 2).
trait_source(cleric(life) >: 17, 'supreme healing').
bonus_source(trait('supreme healing'),
             modify_spell(_, HealingSpell, apply_supreme_healing)) :-
    spell_property(HealingSpell, effects, Effects),
    subterm_member(heal(Formula), Effects),
    subterm_member(_ d _, Formula). % Only applies to healing spells for which you have to roll.
apply_supreme_healing(Old, Old.put(effects, NewEffects)) :-
    map_matching_subterms(heal_roll_max, Old.get(effects), NewEffects).
heal_roll_max(heal(Formula), heal(NewFormula)) :-
    map_matching_subterms(roll_max, Formula, MaxedRolls),
    simplify_dice_sum(MaxedRolls, NewFormula).


% Knowledge domain.
% -----------------
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
trait_source(cleric(knowledge) >: 1, 'blessings of knowledge').
trait_options(trait('blessings of knowledge'), language, wrap(skill),
              2 unique_from language) :-
    cleric(knowledge) >: 1.
trait_options(trait('blessings of knowledge'), skill, wrap(skill),
              2 unique_from from_list([arcana,history,nature,religion])) :-
    cleric(knowledge) >: 1.
trait_source(trait('blessings of knowledge'), expertise(skill(Skill))) :-
    choice_member(trait('blessings of knowledge'), skill, Skill).
meta_todo(trait('blessings of knowledge'), "Technically it's not expertise, but mechanically I'm not sure it's worth making a distinction here. In particular if it's not expertise, I'm not sure how/whether it stacks with expertise.").

% Knowledge of the ages.
trait_source(cleric(knowledge) >: 2, 'knowledge of the ages').
trait_source(cleric(knowledge) >: 6, channel_divinity('read thoughts')).

trait_source(cleric(knowledge) >: 8, 'potent spellcasting'(cleric)).
bonus_source(trait('potent spellcasting'(cleric)), modify_spell(_, Cantrip, Goal)) :-
    known_spell(_, Cantrip),
    spell_data(Cantrip, Data),
    Data.level = 0,
    subterm_member(damage(_,_), Data.effects),
    ability_mod(wis, Bonus),
    Goal = modify_spell_field(effects, apply_potent_spellcasting(Bonus)).
apply_potent_spellcasting(Bonus, OldEffects, NewEffects) :-
    select_subterm(damage(Element, Dice), OldEffects,
                   damage(Element, NewDice), NewEffects),
    simplify_dice_sum(Dice + Bonus, NewDice).
custom_format(modify_spell_field(effects, apply_potent_spellcasting(Bonus))) -->
    ["+"], [Bonus], [" damage"].

trait_source(cleric(knowledge) >: 17, 'visions of the past').

meta_todo(cleric(knowledge), "Complete this subclass implementation.").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% DESCRIPTIONS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

'channel divinity (cleric)' ?= "At 2nd level, you gain the ability to channel divine energy directly from your deity, using that energy to fuel magical effects. You start with two such effects: Turn Undead and an effect determined by your domain. Some domains grant you additional effects as you advance in levels, as noted in the domain description.
When you use your Channel Divinity, you choose which effect to create. You must then finish a short or long rest to use your Channel Divinity again. Some Channel Divinity effects require saving throws. When you use such an effect from this class, the DC equals your cleric spell save DC.
Beginning at 6th level, you can use your Channel Divinity twice between rests, and beginning at 18th level, you can use it three times between rests. When you finish a short or long rest, you regain your expended uses.".

channel_divinity('turn undead') ?= "As an action, you present your holy symbol and speak a prayer censuring the undead. Each undead that can see or hear you within 30 feet of you must make a Wisdom saving throw. If the creature fails its saving throw, it is turned for 1 minute or until it takes any damage.
A turned creature must spend its turns trying to move as far away from you as it can, and it can't willingly move to a space within 30 feet of you. It also can't take reactions. For its action, it can use only the Dash action or try to escape from an effect that prevents it from moving. If there's nowhere to move, the creature can use the Dodge action.".

channel_divinity('knowledge of the ages') ?= "Starting at 2nd level, you can use your Channel Divinity to tap into a divine well of knowledge. As an action, you choose one skill or tool. For 10 minutes, you have proficiency with the chosen skill or tool.".

channel_divinity('read thoughts') ?= "At 6th level, you can use your Channel Divinity to read a creature's thoughts. You can then use your access to the creature's mind to command it.

As an action, choose one creature that you can see within 60 feet of you. That creature must make a Wisdom saving throw. If the creature succeeds on the saving throw, you can't use this feature on it again until you finish a long rest.

If the creature fails its save, you can read its surface thoughts (those foremost in its mind, reflecting its current emotions and what it is actively thinking about) when it is within 60 feet of you. This effect lasts for 1 minute.

During that time, you can use your action to end this effect and cast the Suggestion spell on the creature without expending a spell slot. The target automatically fails its saving throw against the spell.".

'potent spellcasting' ?= "Starting at 8th level, you add your Wisdom modifier to the damage you deal with any cleric cantrip.".

'visions of the past' ?= "Starting at 17th level, you can call up visions of the past that relate to an object you hold or your immediate surroundings. You spend at least 1 minute in meditation and prayer, then receive dreamlike, shadowy glimpses of recent events. You can meditate in this way for a number of minutes equal to your Wisdom score and must maintain concentration during that time, as if you were casting a spell.

Once you use this feature, you can’t use it again until you finish a short or long rest.

**Object Reading.** Holding an object as you meditate, you can see visions of the object’s previous owner. After meditating for 1 minute, you learn how the owner acquired and lost the object, as well as the most recent significant event involving the object and that owner. If the object was owned by another creature in the recent past (within a number of days equal to your Wisdom score), you can spend 1 additional minute for each owner to learn the same information about that creature.

**Area Reading.** As you meditate, you see visions of recent events in your immediate vicinity (a room, street, tunnel, clearing, or the like, up to a 50-foot cube), going back a number of days equal to your Wisdom score. For each minute you meditate, you learn about one significant event, beginning with the most recent. Significant events typically involve powerful emotions, such as battles and betrayals, marriages and murders, births and funerals. However, they might also include more mundane events that are nevertheless important in your current situation.".

destroy_undead(_) ?= "Starting at 5th level, when an undead fails its saving throw against your Turn Undead feature, the creature is instantly destroyed if its challenge rating is at or below a certain threshold, as shown in the Destroy Undead table.".

divine_intervention(_) ?= "Beginning at 10th level, you can call on your deity to intervene on your behalf when your need is great. Imploring your deity's aid requires you to use your action. Describe the assistance you seek, and roll percentile dice. If you roll a number equal to or lower than your cleric level, your deity intervenes. The GM chooses the nature of the intervention; the effect of any cleric spell or cleric domain spell would be appropriate. If your deity intervenes, you can't use this feature again for 7 days. Otherwise, you can use it again after you finish a long rest. At 20th level, your call for intervention succeeds automatically, no roll required.".

'disciple of life' ?= "Starting at 1st level, your healing spells are more effective. Whenever you use a spell of 1st level or higher to restore hit points to a creature, the creature regains additional hit points equal to 2 + the spell's level.".

'blessed healer' ?= "Beginning at 6th level, the healing spells you cast on others heal you as well. When you cast a spell of 1st level or higher that restores hit points to a creature other than you, you regain hit points equal to 2 + the spell's level.".

'divine strike' ?= "At 8th level, you gain the ability to infuse your weapon strikes with divine energy. Once on each of your turns when you hit a creature with a weapon attack, you can cause the attack to deal an extra 1d8 radiant damage to the target. When you reach 14th level, the extra damage increases to 2d8.".

channel_divinity(preserve_life(_)) ?= "Starting at 2nd level, you can use your Channel Divinity to heal the badly injured.
As an action, you present your holy symbol and evoke healing energy that can restore a number of hit points equal to five times your cleric level. Choose any creatures within 30 feet of you, and divide those hit points among them. This feature can restore a creature to no more than half of its hit point maximum. You can't use this feature on an undead or a construct. ".

'supreme healing' ?= "Starting at 17th level, when you would normally roll one or more dice to restore hit points with a spell, you instead use the highest number possible for each die. For example, instead of restoring 2d6 hit points to a creature, you restore 12.".

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

'channel divinity (cleric)' @= srd('164').
channel_divinity('turn undead') @= srd('59').
channel_divinity('read thoughts') @= srd('59-60').
destroy_undead(_) @= srd('59').

divine_intervention(_)@=srd('59').
'disciple of life'@=srd('60').
'blessed healer'@=srd('60').
divine_strike(_)@=srd(null).
'supreme healing'@=srd('60').
'blessings of knowledge'@=phb('59').

