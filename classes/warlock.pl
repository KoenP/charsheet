class_option(warlock).
hd_per_level(warlock, 1 d 8).
initial_class_base_hp(warlock, 8).
max_hp_per_level(warlock, 1 d 8).
class_saving_throw(warlock, wis).
class_saving_throw(warlock, cha).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

class_trait(warlock:1, armor(light)).
class_trait(warlock:1, weapon(simple)).




%class_trait_options(warlock:1, skill,
%                    2 from [ arcana, deception, history,
%                             intimidation, investigation,
%                             nature, religion ]).
%%choice_to_trait(warlock:1, skill, S, skill(S)).
%class_trait_options(warlock:1, skill,
%                    2 from [ arcana, deception, history,
%                             intimidation, investigation, nature,
%                             religion]).
%menu_trait(warlock:1, skill, Choice, skill(Choice)).
