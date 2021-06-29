
feature(race(tortle), add_ability(str, 2)).
feature(race(tortle), add_ability(wis, 1)).
feature(race(tortle), size(medium)).
feature(race(tortle), claws). % todo
feature(race(tortle), hold_breath).
feature(race(tortle), natural_armor(17)).
feature(race(tortle), shell_defense).
feature(race(tortle), proficient(survival)).
feature(race(tortle), language(common)).
feature(race(tortle), language(aquan)).

describe(shell_defense, "You can withdraw into your shell as an action. Until you emerge, you gain a +4 bonus to AC, and you have advantage on Strength and Constitution saving throws. While in your shell, you are prone, your speed is 0 and can't increase, you have disadvantage on Dexterity saving throws, you can't take reactions, and the only action you can take is a bonus action to emerge from your shell.").
