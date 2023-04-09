class_option(paladin).
hd_per_level(paladin, 1 d 10).
initial_class_base_hp(paladin, 10).
max_hp_per_level(paladin, 1 d 10).
caster(paladin, 1/2).
choose_subclass_level(paladin:3).
asi_level(paladin:L) :-
    default_asi_level(L).
class_saving_throw(paladin, wis).
class_saving_throw(paladin, cha).
