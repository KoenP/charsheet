race_option(tabaxi).
race_shorthand(tabaxi, tx).
racial_speed(tabaxi, walking, 30).
racial_speed(tabaxi, climbing, 30).

grant_racial_asis_plus2_plus1(tabaxi).

trait_source(race(tabaxi), sense(darkvision)).
trait_source(race(tabaxi), 'feline agility').
trait_source(race(tabaxi), 'cat\'s claws').

% Tabaxi claws are a natural weapon, but it is explicitly stated that it can
% be used for unarmed strikes. So any bonus to unarmed strikes should also apply
% to tabaxi claws.
% Nevertheless I think claws should not *override* regular unarmed strikes:
% a tabaxi can still kick, headbutt, etc for bludgeoning damage.
% The interaction with monk's martial arts feature is tricky:
% I think the correct interpretation is that tabaxi have two variants of unarmed
% strike, one that deals 1d4+str slashing and one that deals 1+str bludgeoning;
% and *both* of these variants have their damage formulae replaced by the martial
% arts formula (but retain their damage type). So in practice a tabaxi monk
% deals the same damage as any other, but with the option to pick between
% slashing and bludgeoning.
weapon('cat\'s claws', natural, melee, [damage(slashing, 1 d 4)], []).
counts_as_unarmed('cat\'s claws').
has_natural_weapon('cat\'s claws') :- trait(race(tabaxi), 'cat\'s claws').

traits_from_source(race(tabaxi),
                   [skill(perception), skill(stealth), language(common)]).
trait_options_source(race(tabaxi), language, wrap(language), language).


'feline agility' ?= "Your reflexes and agility allow you to move with a burst of speed. When you move on your turn in combat, you can double your speed until the end of the tum. Once you use this trait, you can't use it again until you move 0 feet on one of your turns.".

'cat\'s claws' ?= "Because of your claws, you have a climbing speed of 20 feet. In addition, your claws are natural weapons, which you can use to make unarmed strikes. If you hit with them, you deal slashing damage equal to 1d4 + your Strength modifier, instead of the bludgeoning damage normal for an unarmed strike.".
