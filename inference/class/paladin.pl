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
resource('divine sense', 'divine sense', N) :-
    trait('divine sense'), ability_mod(cha, Mod), N is max(0, Mod+1).
on_rest(long, 'divine sense', full_restore).

% Lay on hands.
trait_source(paladin >: 1, 'lay on hands').
resource('lay on hands', 'hit points', HP) :-
    class_level(paladin:L), HP is 5*L.
on_rest(long, 'lay on hands', full_restore).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Features gained from leveling up.
trait_options_source(paladin >: 2, 'fighting style',
                     wrap(fighting_style),
                     from_list([defense,dueling,'great weapon fighting',
                                protection])).

trait_source(paladin >: 2, 'divine smite').
trait_source(paladin >: 3, 'divine health').

trait_source(paladin >: 3, 'channel divinity').
bonus_source(paladin >: 3, channel_divinity_uses(1)).

trait_source(paladin >: 5, 'extra attack').
trait_source(paladin >: 6, 'aura of protection').
trait_source(paladin >: 9, 'aura of courage').

trait_source(paladin >: 11, 'improved divine smite').
bonus_source(trait('improved divine smite'),
             extra_damage_roll(BaseWeapon, damage(radiant, 1 d 8))) :-
    weapon(BaseWeapon, _, melee, _, _).

trait_source(paladin >: 13, 'cleansing touch').
resource('cleansing touch', 'cleansing touch', N) :-
    ability_mod(cha, Mod), N is max(1, Mod).
on_rest(long, 'cleansing touch', full_restore).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Spellcasting.

trait_source(paladin >: 1, spellcasting_focus('holy symbol')).

% Paladins know all proper spells on their spell list.
known_spell(paladin, cha, 'when prepared', [slot], Ritual, Spell) :-
    learnable_proper_spell(paladin, Spell),
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
resource('holy nimbus', 'holy nimbus', 1) :-
    trait('holy nimbus').
on_rest(long, 'holy nimbus', full_restore).
