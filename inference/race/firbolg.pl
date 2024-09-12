race_option(firbolg).
racial_speed(firbolg, walking, 30).
race_shorthand(firbolg, fb).
grant_racial_asis_plus2_plus1(firbolg).

% Languages.
trait_source(race(firbolg), language(common)).
trait_options_source(race(firbolg), language, wrap(language), language).

% Firbolg magic.
known_spell(firbolg, Abi, always, ['firbolg spell slot' or slot], no, Spell) :-
    race(firbolg),
    spellcasting_ability(firbolg, Abi),
    (Spell = 'detect magic' ; Spell = 'disguise self').
res('firbolg spell slot', 1) :-
    race(firbolg).
on_rest(long, 'firbolg spell slot', 'full restore').
bonus_source(race(firbolg), modify_spell(firbolg, 'disguise self', Goal)) :-
    Goal = add_spell_effects(["You can appear up to 3 ft shorter or taller"]).

options_source(race(firbolg), 'firbolg spellcasting ability', from_list([int, wis, cha])).
spellcasting_ability(firbolg, Abi) :-
    choice(race(firbolg), 'firbolg spellcasting ability', Abi).

% Hidden step.
trait_source(race(firbolg), 'hidden step').
res('hidden step', ProfBon) :-
    race(firbolg),
    proficiency_bonus(ProfBon).
on_rest(long, 'hidden step', 'full restore').

% Other features.
traits_from_source(race(firbolg), ['powerful build', 'speech of beast and leaf']).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

'firbolg magic' ?= "You can cast the detect magic and disguise self spells with this trait. When you use this version of disguise self, you can seem up to 3 feet shorter or taller. Once you cast either of these spells with this trait, you can’t cast that spell with it again until you finish a long rest. You can also cast these spells using any spell slots you have.

Intelligence, Wisdom, or Charisma is your spellcasting ability for these spells when you cast them with this trait (choose when you select this race).".

'hidden step' ?= "As a bonus action, you can magically turn invisible until the start of your next turn or until you attack, make a damage roll, or force someone to make a saving throw. You can use this trait a number of times equal to your proficiency bonus, and you regain all expended uses when you finish a long rest.".

'powerful build' ?= "You count as one size larger when determining your carrying capacity and the weight you can push, drag, or lift.".

'speech of beast and leaf' ?= "You have the ability to communicate in a limited manner with Beasts, Plants, and vegetation. They can understand the meaning of your words, though you have no special ability to understand them in return. You have advantage on all Charisma checks you make to influence them.".
