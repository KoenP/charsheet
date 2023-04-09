class_option(monk).
hd_per_level(monk, 1 d 8).
initial_class_base_hp(monk, 8).
max_hp_per_level(monk, 1 d 8).
caster(monk, 0).
choose_subclass_level(monk:3).
asi_level(monk:L) :-
    default_asi_level(L).
class_saving_throw(monk, str).
class_saving_throw(monk, dex).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Initial class features
class_skill_list(monk,
                 [acrobatics, athletics, history, insight, religion, stealth]).
trait_options_source(^monk, skill, wrap(skill), 2 unique_from class_skill(monk)).
trait_options_source(^monk, 'tool or instrument', id,
                     artisans_tool or musical_instrument).
meta_todo(artisans_tool, "make an artisans_tool predicate").
meta_todo(musical_instrument, "make a musical_instrument predicate").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Level 1 features.
traits_from_source(monk >: 1, [weapon(simple), weapon(shortsword)]).

trait_source(monk >: 1, unarmored_defense(10 + dex + wis)).

% Martial arts.
trait_source(monk >: 1, martial_arts(1 d N)) :-
    class_level(monk:L),
    ordered_lookup_largest_leq([1 -> 4, 5 -> 6, 11 -> 8, 17 -> 10], L, N).
monk_weapon(unarmed).
monk_weapon('short sword').
monk_weapon(Weapon) :-
    weapon(Weapon, simple, _, _, Notes),
    intersection([heavy, twohanded], Notes, []).
bonus_source(trait(martial_arts(_)), use_ability(Weapon, dex)) :-
    monk_weapon(Weapon).
bonus(trait(martial_arts(1 d N)),
      attack_damage_rolls(Weapon,[damage(Type,1 d N)])) :-
    trait(martial_arts(1 d N)),
    monk_weapon(Weapon),
    weapon(Weapon, _, _, [damage(Type,Die)], _),
    ((Die = 1 d M, N > M) ; Die = 1).
bonus_source(trait(martial_arts(_)),
             add_weapon_note(Weapon, "+ unarmed strike as bonus action")) :-
    monk_weapon(Weapon).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Features gained from leveling up.

% Ki features.
trait_source(monk >: 2, ki(N, dc(DC))) :-
    class_level(monk:N),
    proficiency_bonus(ProfBon),
    ability_mod(wis, WisMod),
    DC is 8 + ProfBon + WisMod.
traits_from_source(monk >: 2, [ki_feature('flurry of blows'),
                               ki_feature('patient defense'),
                               ki_feature('step of the wind')]).
resource(ki, 'ki points', N) :-
    trait(ki(N, _)).
on_rest(short, ki, full_restore).

% Unarmored movement.
trait_source(monk >: 2, unarmored_movement(ft(Ft))) :-
    class_level(monk:L),
    Ft is 10 + 5 * floor((L-2) / 4).
bonus_source(trait(unarmored_movement(ft(Ft))), speed + Ft).
trait_source(monk >: 9, unarmored_movement("vertical surfaces and liquids")).

% ...
trait_source(monk >: 3, deflect_missiles(1 d 10 + Red)) :-
    ability_mod(dex, Mod),
    class_level(monk:L),
    Red is Mod + L.
trait_source(trait(deflect_missiles(_)), ki_feature('reflect missile')).

trait_source(monk >: 4, slow_fall(Red)) :-
    class_level(monk:L),
    Red is L*5.

trait_source(monk >: 5, extra_attack(1)).

trait_source(monk >: 5, ki_feature('stunning strike')).

trait_source(monk >: 6, 'ki-empowered strikes').
bonus_source(trait('ki-empowered strikes'), add_weapon_note(unarmed, magical)).

trait_source(monk >: 7, evasion).

trait_source(monk >: 7, 'stillness of mind').

trait_source(monk >: 10, 'purity of body').

trait_source(monk >: 13, 'tongue of the sun and moon').

trait_source(monk >: 14, 'diamond soul').
trait_source(trait('diamond soul'), saving_throw(Abi)) :- ability(Abi).

trait_source(monk >: 15, 'timeless body').

trait_source(monk >: 18, ki_feature('empty body')).

trait_source(monk >: 20, 'perfect self').
meta_todo('perfect self', "encode the regain of ki points").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% MONASTIC TRADITIONS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Way of the Open Hand
% --------------------
subclass_option(monk, 'open hand').

trait_source(monk('open hand') >: 3, 'open hand technique').

trait_source(monk('open hand') >: 6, wholeness_of_body(hp(HP))) :-
    class_level(monk:L),
    HP is L*3.
resource('wholeness of body', 'wholeness of body', 1) :-
    trait(wholeness_of_body(_)).
on_rest(long, 'wholeness of body', full_restore).

trait_source(monk('open hand') >: 11, tranquility(dc(DC))) :-
    add_ability_mod_and_profbon(8, wis, DC).

trait_source(monk('open hand') >: 17, 'quivering palm').

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% DESCRIPTIONS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
meta_todo(monk, "most descriptions").

unarmored_movement(ft(_)) ?= "Starting at 2nd level, your speed increases by 10 feet while you are not wearing armor or wielding a shield. This bonus increases when you reach certain monk levels, as shown in the Monk table.".
unarmored_movement("vertical surfaces and liquids") ?= "At 9th level, you gain the ability to move along vertical surfaces and across liquids on your turn without falling during the move.".

ki_feature('reflect missile') ?= "Starting at 3rd level, you can use your reaction to deflect or catch the missile when you are hit by a ranged weapon attack. When you do so, the damage you take from the attack is reduced by 1d10 + your Dexterity modifier + your monk level.
If you reduce the damage to 0, you can catch the missile if it is small enough for you to hold in one hand and you have at least one hand free. If you catch a missile in this way, you can spend 1 ki point to make a ranged attack with the weapon or piece of ammunition you just caught, as part of the same reaction. You make this attack with proficiency, regardless of your weapon proficiencies, and the missile counts as a monk weapon for the attack, which has a normal range of 20 feet and a long range of 60 feet.".
