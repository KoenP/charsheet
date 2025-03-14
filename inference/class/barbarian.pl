class_option(barbarian).
hd_per_level(barbarian, 1 d 12).
initial_class_base_hp(barbarian, 12).
max_hp_per_level(barbarian, 1 d 12).
caster(barbarian, 0).
choose_subclass_level(barbarian:3).
class_saving_throw(barbarian, str).
class_saving_throw(barbarian, con).
asi_level(barbarian:L) :- default_asi_level(L).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Initial class features (you don't get these when multiclassing into barbarian).
class_skill_list(barbarian,
                 ['animal handling', athletics, intimidation,
                   nature, perception, survival]).

traits_from_source(^barbarian, [armor(light), armor(medium)]).
trait_options_source(^barbarian, skill, wrap(skill),
                     2 unique_from class_skill(barbarian)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Level 1 features (you get these when multiclassing into barbarian).
traits_from_source(barbarian >: 1,
                   [armor(shield), weapon(simple), weapon(martial)]).

trait_source(barbarian >: 1, rage(damage + N)) :-
    class_level(barbarian:L),
    ordered_lookup_largest_leq([1 -> 2, 9 -> 3, 16 -> 4], L, N).
res(rage, Max) :-
    class_level(barbarian:L),
    ordered_lookup_largest_leq([1 -> 2,
                                3 -> 3,
                                6 -> 4,
                                12 -> 5,
                                17 -> 6,
                                20 -> unlimited], L, Max).
meta_todo(barbarian, "how to handle 'unlimited' rages in UI").
restore_res('long rest', rage, 'full restore') :- trait(rage).

trait_source(barbarian >: 1, unarmored_defense(10 + dex + con + shield)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Features gained from leveling up.
trait_source(barbarian >: 2, 'reckless attack').
trait_source(barbarian >: 2, 'danger sense').
multiclass_trait_source(barbarian >: 5, extra_attack(1)).
trait_source(barbarian >: 5, 'fast movement').
bonus_source(trait('fast movement'), speed + 10).
trait_source(barbarian >: 7, 'feral instinct').
trait_source(barbarian >: 9, brutal_critical(N)) :-
    class_level(barbarian:L),
    ordered_lookup_largest_leq([9->1, 13->2, 17->3], L, N).
trait_source(barbarian >: 11, 'relentless rage').
trait_source(barbarian >: 15, 'persistent rage').
trait_source(barbarian >: 18, 'indomitable might').
trait_source(barbarian >: 20, 'primal champion').
bonus_source(trait('primal champion'), str + 4).
bonus_source(trait('primal champion'), con + 4).
bonus_source(trait('primal champion'), max_ability(str) + 4).
bonus_source(trait('primal champion'), max_ability(con) + 4).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% SUBCLASSES                                                                   %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
subclass_option(barbarian, berserker).
trait_source(barbarian(berserker) >: 3, frenzy).
trait_source(barbarian(berserker) >: 6, 'mindless rage').
trait_source(barbarian(berserker) >: 10, 'intimidating presence').
trait_source(barbarian(berserker) >: 14, retaliation).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% DESCRIPTIONS                                                                 %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

rage(_) ?= "In battle, you fight with primal ferocity. On your turn, you can enter a rage as a bonus action.

While raging, you gain the following benefits if you aren't wearing heavy armor:

- You have advantage on Strength checks and Strength saving throws.
- When you make a melee weapon attack using Strength, you gain a bonus to the damage roll that increases as you gain levels as a barbarian, as shown in the Rage Damage column of the Barbarian table.
- You have resistance to bludgeoning, piercing, and slashing damage.

If you are able to cast spells, you can't cast them or concentrate on them while raging.

Your rage lasts for 1 minute. It ends early if you are knocked unconscious or if your turn ends and you haven't attacked a hostile creature since your last turn or taken damage since then. You can also end your rage on your turn as a bonus action.

Once you have raged the number of times shown for your barbarian level in the Rages column of the Barbarian table, you must finish a long rest before you can rage again.".

'unarmored defense' ?= "While you are not wearing any armor, your Armor Class equals 10 + your Dexterity modifier + your Constitution modifier. You can use a shield and still gain this benefit.".

'reckless attack' ?= "Starting at 2nd level, you can throw aside all concern for defense to attack with fierce desperation. When you make your first attack on your turn, you can decide to attack recklessly. Doing so gives you advantage on melee weapon attack rolls using Strength during this turn, but attack rolls against you have advantage until your next turn.".

'danger sense' ?= "At 2nd level, you gain an uncanny sense of when things nearby aren't as they should be, giving you an edge when you dodge away from danger. You have advantage on Dexterity saving throws against effects that you can see, such as traps and spells. To gain this benefit, you can’t be blinded, deafened, or incapacitated.".

'fast movement' ?= "Starting at 5th level, your speed increases by 10 feet while you aren’t wearing heavy armor.".

'feral instinct' ?= "By 7th level, your instincts are so honed that you have advantage on initiative rolls.

Additionally, if you are surprised at the beginning of combat and aren't incapacitated, you can act normally on your first turn, but only if you enter your rage before doing anything else on that turn.".

'brutal critical' ?= "Beginning at 9th level, you can roll one additional weapon damage die when determining the extra damage for a critical hit with a melee attack. This increases to two additional dice at 13th level and three additional dice at 17th level.".

'relentless rage' ?= "Starting at 11th level, your rage can keep you fighting despite grievous wounds. If you drop to 0 hit points while you're raging and don't die outright, you can make a DC 10 Constitution saving throw. If you succeed, you drop to 1 hit point instead.

Each time you use this feature after the first, the DC increases by 5. When you finish a short or long rest, the DC resets to 10.".

'persistent rage' ?= "Beginning at 15th level, your rage is so fierce that it ends early only if you fall unconscious or if you choose to end it.".

'indomitable might' ?= "Beginning at 18th level, if your total for a Strength check is less than your Strength score, you can use that score in place of the total.".

'primal champion' ?= "At 20th level, you embody the power of the wilds. Your Strength and Constitution scores increase by 4. Your maximum for those scores is now 24.".

frenzy ?= "Starting when you choose this path at 3rd level, you can go into a frenzy when you rage. If you do so, for the duration of your rage you can make a single melee weapon attack as a bonus action on each of your turns after this one. When your rage ends, you suffer one level of exhaustion (as described in appendix A).".

'mindless rage' ?= "Beginning at 6th level, you can't be charmed or frightened while raging. If you are charmed or frightened when you enter your rage, the effect is suspended for the duration of the rage.".

'intimidating presence' ?= "Beginning at 10th level, you can use your action to frighten someone with your menacing presence. When you do so, choose one creature that you can see within 30 feet of you. If the creature can see or hear you, it must succeed on a Wisdom saving throw (DC equal to 8 + your proficiency bonus + your Charisma modifier) or be frightened of you until the end of your next turn. On subsequent turns, you can use your action to extend the duration of this effect on the frightened creature until the end of your next turn. This effect ends if the creature ends its turn out of line of sight or more than 60 feet away from you.

If the creature succeeds on its saving throw, you can't use this feature on that creature again for 24 hours. ".

retaliation ?= "Starting at 14th level, when you take damage from a creature that is within 5 feet of you, you can use your reaction to make a melee weapon attack against that creature.".

rage(_)@=srd('48').
'reckless attack'@=srd('48').
'danger sense'@=srd('48').
'fast movement'@=srd('49').
'feral instinct'@=srd('49').
brutal_critical(_)@=srd('49').
'relentless rage'@=srd('49').
'persistent rage'@=srd('49').
'indomitable might'@=srd('49').
'primal champion'@=srd('49').
frenzy@=srd('49').
'mindless rage'@=srd('49').
'intimidating presence'@=srd('49-50').
retaliation@=srd('50').
