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
% Level 1 features
traits_from_source(monk >: 1, [weapon(simple), weapon(shortsword)]).

trait_source(monk >: 1, unarmored_defense(10 + dex + wis)).

trait_source(monk >: 1, martial_arts(1 d N)) :-
    class_level(monk:L),
    ordered_lookup_largest_leq([1 -> 4, 5 -> 6, 11 -> 8, 17 -> 10], L, N).
monk_weapon('short sword').
monk_weapon(Weapon) :-
    weapon(Weapon, simple, _, _, Notes),
    intersection([heavy, twohanded], Notes, []).
%bonus_source(trait(martial_arts(Die)), use_ability(Weapon, dex)) :-
%    monk_weapon(Weapon).
% TODO hier zat ik

%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Features gained from leveling up.
%trait_source(monk >: 2, 'ki').
%resource('ki', 'ki point', N) :-
%    trait('ki'),
%    ability_mod(wis, N).
%trait_source(monk >: 2, 'unarmored movement').
%trait_source(monk >: 2, 'patient defense').
%trait_source(monk >: 2, 'step of the wind').
%on_action(monk >: 2, 'step of the wind', [
%    additional_move(2 * speed),
%    additional_action(dash),
%    ignore_difficult_terrain(true)
%    ]).
%trait_options_source(monk >: 3, 'monastic tradition', wrap(monastic_tradition),
%                     1 unique_from monastic_tradition).
%
%trait_source(monk >: 5, 'extra attack').
%
%trait_source(monk >: 6, 'ki-empowered strikes').
%
%trait_source(monk >: 7, 'stillness of mind').
%
%trait_source(monk >: 10, 'purity of body').
%
%trait_source(monk >: 13, 'tongue of the sun and moon').
%
%trait_source(monk >: 14, 'diamond soul').
%
%trait_source(monk >: 18, 'empty body').
%
%trait_source(monk >: 20, 'perfect self').
