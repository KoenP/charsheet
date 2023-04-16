% fey touched -> +1 wis, misty step, sleep

equipped('rapier').
equipped('longbow').
equipped('studded leather').

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

name("Baba Anoesh").

base_ability(str,9).
base_ability(dex,15).
base_ability(con,11).
base_ability(int,11).
base_ability(wis,14).
base_ability(cha,8).

choice(init, 'base race', human).
choice(race(human), subrace, variant).
choice(race(human), language, 'deep speech').
choice(race(human(variant)), skill, acrobatics).
choice(race(human(variant)), feat, 'fey touched').
choice(feat('fey touched'), asi, wis + 1).
choice(feat('fey touched'), spell, sleep).
choice(race(human(variant)), asi, [dex + 1, wis + 1]).

choice(init, 'initial class', ranger).
choice(initial_class(ranger), skill, [stealth, perception, insight]).

choice(init, background, outlander).
choice(background(outlander), language, sylvan).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
gain_level(2, ranger, hp_avg).
choice(match_class(ranger:2), 'fighting style', archery).
choice(match_class(ranger:2), spell, ['hunter\'s mark', 'cure wounds']).

trait(custom, expertise(skill(perception))).
trait(custom, language(elvish)).
trait(custom, language(halfling)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

'favored foe' ?= "When you hit a creature with an attack roll, you can call on your mystical bond with nature to mark the target as your favored enemy for 1 minute or until you lose your concentration (as if you were concentrating on a spell).

The first time on each of your turns that you hit the favored enemy and deal damage to it, including when you mark it, you increase that damage by 1d4.

You can use this feature to mark a favored enemy a number of times equal to your proficiency bonus, and you regain all expended uses when you finish a long rest.

This feature's extra damage increases when you reach certain levels in this class: to 1d6 at 6th level and to 1d8 at 14th level.".


feat('fey touched') ?= "Your exposure to the Feywild's magic has changed you, granting you the following benefits:

    Increase your Intelligence, Wisdom, or Charisma score by 1, to a maximum of 20.

    You learn the Misty Step spell and one 1st-level spell of your choice. The 1st-level spell must be from the Divination or Enchantment school of magic. You can cast each of these spells without expending a spell slot. Once you cast either of these spells in this way, you can’t cast that spell in this way again until you finish a long rest. You can also cast these spells using spell slots you have of the appropriate level. The spells’ spellcasting ability is the ability increased by this feat.".
