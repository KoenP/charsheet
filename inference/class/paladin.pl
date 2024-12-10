class_option(paladin).
hd_per_level(paladin, 1 d 10).
initial_class_base_hp(paladin, 10).
max_hp_per_level(paladin, 1 d 10).
caster(paladin, 1/2).
spellcasting_ability(paladin, cha).
max_prepared_spells(paladin, N) :-
    default_max_prepared_spells(paladin, N).
choose_subclass_level(paladin:3).
asi_level(paladin:L) :-
    default_asi_level(L).
class_saving_throw(paladin, wis).
class_saving_throw(paladin, cha).

class_skill_list(paladin, [athletics, insight, intimidation,
                           medicine, persuasion, religion]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Initial class features (don't get these multiclassing into paladin).
trait_source(^paladin, armor(heavy)).
trait_options_source(^paladin, skill, wrap(skill),
                     2 unique_from class_skill(paladin)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Level 1 features (you get these when multiclassing into paladin).
traits_from_source(paladin >: 1,
                   [weapon(simple), weapon(martial),
                    armor(light), armor(medium), armor(shield)]).

% Divine sense.
trait_source(paladin >: 1, 'divine sense').
res('divine sense', N) :-
    trait('divine sense'), ability_mod(cha, Mod), N is max(0, Mod+1).
restore_res('long rest', 'divine sense', 'full restore').

% Lay on hands.
trait_source(paladin >: 1, 'lay on hands').
res('lay on hands', HP) :-
    class_level(paladin:L), HP is 5*L.
restore_res('long rest', 'lay on hands', 'full restore').

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Features gained from leveling up.
trait_options_source(paladin >: 2, 'fighting style',
                     wrap(fighting_style),
                     from_list([defense,dueling,'great weapon fighting',
                                protection])).
%lookup_option_doc(paladin >: 2, 'fighting style', Style, Doc) :-
%    (fighting_style(Style) ?= Doc).

trait_source(paladin >: 2, 'divine smite').
trait_source(paladin >: 3, 'divine health').

trait_source(paladin >: 3, 'channel divinity').
bonus_source(paladin >: 3, channel_divinity_uses(1)).

multiclass_trait_source(paladin >: 5, extra_attack(1)).
trait_source(paladin >: 6, 'aura of protection').
trait_source(paladin >: 10, 'aura of courage').

trait_source(paladin >: 11, 'improved divine smite').
bonus_source(trait('improved divine smite'),
             extra_damage_roll(BaseWeapon, damage(radiant, 1 d 8))) :-
    weapon(BaseWeapon, _, melee, _, _).

trait_source(paladin >: 13, 'cleansing touch').
res('cleansing touch', N) :-
    trait('cleansing touch'),
    ability_mod(cha, Mod), N is max(1, Mod).
restore_res('long rest', 'cleansing touch', 'full restore').

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Spellcasting.

trait_source(paladin >: 1, spellcasting_focus('holy symbol')).

% Paladins know all proper spells on their spell list.
known_spell(paladin, cha, 'when prepared', [slot], Ritual, Spell) :-
    learnable_proper_spell(paladin, Spell),
    % Exclude oath spells so they don't show up twice.
    \+ (subclass(paladin(Oath)), paladin_oath_spell(Oath, Spell)),
    spell_property(Spell, ritual, Ritual).

% Oath spells are always prepared.
known_spell(paladin(Oath), cha, always, [slot], Ritual, Spell) :-
    subclass(paladin(Oath)),
    paladin_oath_spell(Oath, Spell),
    learnable_proper_spell(paladin, Spell),
    spell_property(Spell, ritual, Ritual).

% Add oath spells to the paladin spell list.
extend_class_spell_list(paladin, Spell) :-
    subclass(paladin(Oath)),
    paladin_oath_spell(Oath, Spell).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% OATHS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Oath of Devotion
subclass_option(paladin, devotion).

paladin_oath_spell(devotion, 'protection from evil and good').
paladin_oath_spell(devotion, sanctuary).
paladin_oath_spell(devotion, 'lesser restoration').
paladin_oath_spell(devotion, 'zone of truth').
paladin_oath_spell(devotion, 'beacon of hope').
paladin_oath_spell(devotion, 'dispel magic').
paladin_oath_spell(devotion, 'freedom of movement').
paladin_oath_spell(devotion, 'guardian of faith').
paladin_oath_spell(devotion, commune).
paladin_oath_spell(devotion, 'flame strike').

trait_source(paladin(devotion) >: 3, channel_divinity('sacred weapon')).
trait_source(paladin(devotion) >: 3, channel_divinity('turn the unholy')).
trait_source(paladin(devotion) >: 7, aura_of_devotion(feet(Dst))) :-
    (paladin(devotion) >: 18) -> (Dst = 30) ; (Dst = 10).
trait_source(paladin(devotion) >: 15, 'purity of spirit').
trait_source(paladin(devotion) >: 20, 'holy nimbus').
res('holy nimbus', 1) :-
    trait('holy nimbus').
restore_res('long rest', 'holy nimbus', 'full restore').

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

'divine sense' ?= "The presence of strong evil registers on your senses like a noxious odor, and powerful good rings like heavenly music in your ears. As an action, you can open your awareness to detect such forces. Until the end of your next turn, you know the location of any celestial, fiend, or undead within 60 feet of you that is not behind total cover. You know the type (celestial, fiend, or undead) of any being whose presence you sense, but not its identity (the vampire Count Strahd von Zarovich, for instance). Within the same radius, you also detect the presence of any place or object that has been consecrated or desecrated, as with the hallow spell.
You can use this feature a number of times equal to 1 + your Charisma modifier. When you finish a long rest, you regain all expended uses.".

'lay on hands' ?= "Your blessed touch can heal wounds. You have a pool of healing power that replenishes when you take a long rest. With that pool, you can restore a total number of hit points equal to your paladin level × 5.
As an action, you can touch a creature and draw power from the pool to restore a number of hit points to that creature, up to the maximum amount remaining in your pool.
Alternatively, you can expend 5 hit points from your pool of healing to cure the target of one disease or neutralize one poison affecting it. You can cure multiple diseases and neutralize multiple poisons with a single use of Lay on Hands, expending hit points separately for each one.
This feature has no effect on undead and constructs.".

'divine smite' ?= "Starting at 2nd level, when you hit a creature with a melee weapon attack, you can expend one spell slot to deal radiant damage to the target, in addition to the weapon’s damage. The extra damage is 2d8 for a 1st-level spell slot, plus 1d8 for each spell level higher than 1st, to a maximum of 5d8. The damage increases by 1d8 if the target is an undead or a fiend.".

'divine health' ?= "By 3rd level, the divine magic flowing through you makes you immune to disease.".

'aura of protection' ?= "Starting at 6th level, whenever you or a friendly creature within 10 feet of you must make a saving throw, the creature gains a bonus to the saving throw equal to your Charisma modifier (with a minimum bonus of +1). You must be conscious to grant this bonus.
At 18th level, the range of this aura increases to 30 feet.".

'aura of courage' ?= "Starting at 10th level, you and friendly creatures within 10 feet of you can't be frightened while you are conscious.
At 18th level, the range of this aura increases to 30 feet.".

'improved divine smite' ?= "By 11th level, you are so suffused with righteous might that all your melee weapon strikes carry divine power with them. Whenever you hit a creature with a melee weapon, the creature takes an extra 1d8 radiant damage. If you also use your Divine Smite with an attack, you add this damage to the extra damage of your Divine Smite.".

'cleansing touch' ?= "Beginning at 14th level, you can use your action to end one spell on yourself or on one willing creature that you touch.
You can use this feature a number of times equal to your Charisma modifier (a minimum of once). You regain expended uses when you finish a long rest.".

channel_divinity('sacred weapon') ?= "As an action, you can imbue one weapon that you are holding with positive energy, using your Channel Divinity. For 1 minute, you add your Charisma modifier to attack rolls made with that weapon (with a minimum bonus of +1). The weapon also emits bright light in a 20-foot radius and dim light 20 feet beyond that. If the weapon is not already magical, it becomes magical for the duration.
You can end this effect on your turn as part of any other action. If you are no longer holding or carrying this weapon, or if you fall unconscious, this effect ends.".

channel_divinity('turn the unholy') ?= "As an action, you present your holy symbol and speak a prayer censuring fiends and undead, using your Channel Divinity. Each fiend or undead that can see or hear you within 30 feet of you must make a Wisdom saving throw. If the creature fails its saving throw, it is turned for 1 minute or until it takes damage.
A turned creature must spend its turns trying to move as far away from you as it can, and it can't willingly move to a space within 30 feet of you. It also can’t take reactions. For its action, it can use only the Dash action or try to escape from an effect that prevents it from moving. If there's nowhere to move, the creature can use the Dodge action.".

'aura of devotion' ?= "Starting at 7th level, you and friendly creatures within 10 feet of you can't be charmed while you are conscious.
At 18th level, the range of this aura increases to 30 feet.".

'purity of spirit' ?= "Beginning at 15th level, you are always under the effects of a protection from evil and good spell.".

'holy nimbus' ?= "At 20th level, as an action, you can emanate an aura of sunlight. For 1 minute, bright light shines from you in a 30-foot radius, and dim light shines 30 feet beyond that.
Whenever an enemy creature starts its turn in the bright light, the creature takes 10 radiant damage.
In addition, for the duration, you have advantage on saving throws against spells cast by fiends or undead.
Once you use this feature, you can't use it again until you finish a long rest.".

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

'divine sense'@=srd('84').
'lay on hands'@=srd('84').
'divine smite'@=srd('85').
'divine health'@=srd('85').
'aura of protection'@=srd('85').
'aura of courage'@=srd('85').
'improved divine smite'@=srd('85').
'cleansing touch'@=srd('85').
aura_of_devotion(_)@=srd('86').
'purity of spirit'@=srd('86').
'holy nimbus'@=srd('86').
