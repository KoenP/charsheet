race_option(tabaxi).
race_shorthand(tabaxi, tx).
racial_speed(tabaxi, walking, 30).
racial_speed(tabaxi, climbing, 30).

meta_todo("tabaxi ability_plus_n", "forbid stacking ability plus on same attr").

bonus_options_source(race(tabaxi), 'ability + 2', id, ability_plus_n(2)).
bonus_options_source(race(tabaxi), 'ability + 1', id, ability_plus_n(1)).
hide_base_option(race(tabaxi), 'ability + 1', Abi+1) :-
    bonus(choice(race(tabaxi), 'ability + 2'), Abi+2).
hide_base_option(race(tabaxi), 'ability + 2', Abi+2) :-
    bonus(choice(race(tabaxi), 'ability + 1'), Abi+1).
problem(cant_stack_racial_asis(Abi)) :-
    choice(race(tabaxi), 'ability + 2', Abi + 2),
    choice(race(tabaxi), 'ability + 1', Abi + 1).

meta_todo(race(tabaxi), "generalize ability+1 / ability+2 option").

trait_source(race(tabaxi), sense(darkvision)).
trait_source(race(tabaxi), 'feline agility').
trait_source(race(tabaxi), 'cat\'s claws').

meta_todo("tabaxi claws", "add tabaxi claws").
suppress_unarmed :-
    trait(race(tabaxi), 'cat\'s claws').

traits_from_source(race(tabaxi),
                   [skill(perception), skill(stealth), language(common)]).
trait_options_source(race(tabaxi), language, wrap(language), language).


'feline agility' ?= "Your reflexes and agility allow you to move with a burst of speed. When you move on your tum in combat, you can double your speed until the end of the tum. Once you use this trait, you can't use it again until you move 0 feet on one of your turns.".

'cat\'s claws' ?= "Because of your claws, you have a climbing speed of 20 feet. In addition, your claws are natural weapons, which you can use to make unarmed strikes. If you hit with them, you deal slashing damage equal to 1d4 + your Strength modifier, instead of the bludgeoning damage normal for an unarmed strike.".
