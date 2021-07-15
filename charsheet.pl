:- multifile
       % Indicates problems with your character sheet.
       problem/2,

       % General repository of optional traits your character can have,
       % along with the origin of the trait (trait(Origin, Trait)).
       trait/2,

       % Indicates that a trait should be listed on the character sheet.
       list_trait/1,

       % Item ownership.
       have/1,

       % Documents an attack to appear in the character sheet's attack table.
       % attack(Name, Range, ToHit, Damage, Notes)
       attack/5,

       % A custom chunk of html to add to the character sheet, usually
       % for a specific trait.
       custom_section/1,

       custom_display_rule/2,
       todo/1.

:- table
    problem/1,
    problem/2,
    trait/1,
    trait/2,
    spell_known/5.

:- op(650, xfx, from).
:- op(1000, xfx, ?=).

:- multifile
       (?=)/2.

:- [dice].
:- [options].
:- [spellcasting].
:- [class].
:- [race].
:- [feats].
:- [skills].
:- [leveling].
:- [items].
:- [shorthands].
:- [html].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Traits.
trait(Trait) :- trait(_, Trait).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Character sheet calculations.

% Hit points.
max_hp(HP) :-
    findall(Term, hp_term_for_level(_, Term), Terms),
    sumlist(Terms, HP).

initial_class_base_hp(HP) :-
    initial_class(Class),
    initial_class_base_hp(Class, HP).

hp_term_for_level(1, Term) :-
    initial_class_base_hp(InitHP),
    ability_mod(con, ConMod),
    Term is InitHP + ConMod.
hp_term_for_level(CharLevel, Term) :-
    gain_level(CharLevel, ClassName, HPMode),
    max_hp_per_level(ClassName, HPDie),
    hp_die(HPMode, HPDie, ClassHP),
    ability_mod(con, ConMod),
    Term is ClassHP + ConMod.

hp_die(hp_avg, Die, HP) :-
    die_avg(Die, HP).
hp_die(hp_rolled(HP), _, HP).

hit_dice(HD) :-
    findall(CHD, hit_dice(_, CHD), CHDs),
    list_to_sum(CHDs, Sum),
    simplify_dice_sum(Sum, HD).
hit_dice(Class, M d X) :-
    class_level(Class:Level),
    hd_per_level(Class, N d X),
    M is N * Level.

% Abilities and modifiers.
ability(str).
ability(dex).
ability(con).
ability(wis).
ability(int).
ability(cha).

ability_max(Ability, 20) :- ability(Ability).


% Ability scores are calculated in layers.
% The first layer is base ability + racial bonuses ("naked level 1 ability").
% Then we add asis from level-up, in-order. The first asi that causes an ability score to exceed the maximum (normally 20) raises a problem (this is handled in leveling.pl).
% After that we add asis from feats. If these increases make a score go over 20, it is simply ignored instead of raising an error. TODO: maybe add a "note" or something?
% Item effects are TODO.
naked_lvl1_ability(Ability, Score) :-
    base_ability(Ability, Base),
    findall(Term, racial_asi(Ability+Term), Racial),
    sumlist([Base|Racial], Score).
ability_after_levelup_asis(Ability, Score) :-
    level(Level),
    ability_after_levelup_asis(Level, Ability, Score).
ability_after_levelup_asis(AsiLvl, Ability, Score) :-
    naked_lvl1_ability(Ability, NakedLvl1),
    findall(Term, (between(1,AsiLvl,Level), levelup_asi(Level, Ability+Term)), Terms),
    sumlist([NakedLvl1|Terms], Score).
ability_after_feats(Ability, Score) :-
    ability_after_levelup_asis(Ability, Base),
    findall(Term, trait(feat(_), Ability+Term), Terms),
    sumlist([Base|Terms], Score1),
    ability_max(Ability, Max),
    Score is min(Score1, Max).
ability(Ability, Score) :-
    ability_after_feats(Ability, Score).

% Saving throws.
saving_throw(Ability, ST) :-
    saving_throw_prof(Ability),
    proficiency_bonus(ProfBon),
    ability_mod(Ability, Mod),
    ST is Mod + ProfBon.
saving_throw(Ability, Mod) :-
    ability_mod(Ability, Mod),
    \+ saving_throw_prof(Ability).

% Ability score from base ability, race, abis and feats.
% Not allowed to exceed 20.
%natural_ability(Ability, Score) :-
%    base_ability(Ability, Base),
%    findall(Term, racial_asi(Ability+Term), Racial),
%    findall(Term, levelup_abi(_, Ability+Term), Levelup),
%    append([[Base], Racial, Levelup], All),
%    sumlist(All, Score).

% The order in which the terms are computed matters! 
% It's important for ability score increase error messages.
%ability_score_term(Ability, Term) :-
%    % First we take base ability.
%    base_ability(Ability, Term).
%ability_score_term(Ability, Term) :-
%    % Then we add racial bonuses.
%    racial_trait(_, abi(Ability, Term)).
%ability_score_term(Ability, Term) :-
%    % Then we add level-up abis.
%    false.


mf(Val, Mod) :-
    floor( (Val-10) / 2, Mod ).

:- table ability_mod/2.
ability_mod(Abil, Mod) :-
    ability(Abil, Val),
    mf(Val, Mod).

% Size, speed, ...
size(Size) :-
    race(Race),
    race_size(Race, Size).

speed(Speed) :-
    race(Race),
    race_base_speed(Race, BaseSpeed),
    findall(Term, trait(speed+Term), Terms),
    sum_list([BaseSpeed|Terms], Speed).

% Passive perception.
passive_perception(PP) :-
    skill(perception, P),
    PP is 10 + P.

% Armor class.
ac(AC) :- trait(natural_armor(AC)), !.
ac(AC) :- equipped(Armor), body_armor(Armor, heavy, ac(AC), _), !.
ac(AC) :- equipped(Armor), body_armor(Armor, medium, ac(ArmorAC), _), !,
          ability_mod(dex, Mod),
          AC is ArmorAC + min(Mod,2).
ac(AC) :- equipped(Armor), body_armor(Armor, light, ac(ArmorAC), _), !,
          ability_mod(dex, Mod),
          AC is ArmorAC + Mod.
ac(AC) :- ability_mod(dex, Mod),
          AC is 10 + Mod.

% Initiative.
initiative_mod(Init) :-
    ability_mod(dex, Mod),
    findall(Term, trait(add_initiative(Term)), Terms),
    sumlist([Mod | Terms], Init).

% Proficiency bonus.
calc_bonus(Level, Bonus) :- Bonus is 2 + div(Level-1, 4).
proficiency_bonus(Bonus) :- level(Level), calc_bonus(Level, Bonus).

% Unarmed attacks.
unarmed_attack_modifier(Mod) :-
    proficiency_bonus(Bon),
    ability_mod(str, StrMod),
    Mod is Bon + StrMod.

re :-
    abolish_all_tables,
    make.
