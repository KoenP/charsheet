:- multifile
    trait/2,
    trait_options/4,
    trait_bad_options/5,
    describe/2,
    feature_options/4,
    problem/2,
    initial_class_base_hp/1,
    initial_class_base_hp/2,
    todo/1.

:- [dice].
:- [class].
:- [race].
:- [feats].
:- [skills].
:- [spells].
:- [leveling].
:- [options].
:- [items].
:- [shorthands].

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
% Then we add abis from level-up, in-order. The first abi that causes an ability score to exceed the maximum (normally 20) raises a problem (this is handled in leveling.pl).
% After that we add abis from feats. If these increases make a score go over 20, it is simply ignored instead of raising an error. TODO: maybe add a "note" or something?
% Item effects are TODO.
naked_lvl1_ability(Ability, Score) :-
    base_ability(Ability, Base),
    findall(Term, racial_abi(Ability+Term), Racial),
    sumlist([Base|Racial], Score).
ability_after_levelup_abis(Ability, Score) :-
    level(Level),
    ability_after_levelup_abis(Level, Ability, Score).
ability_after_levelup_abis(AbiLvl, Ability, Score) :-
    naked_lvl1_ability(Ability, NakedLvl1),
    findall(Term, (between(1,AbiLvl,Level), levelup_abi(Level, Ability+Term)), Terms),
    sumlist([NakedLvl1|Terms], Score).
ability_after_feats(Ability, Score) :-
    ability_after_levelup_abis(Ability, Base),
    findall(Term, trait(feat(_), Ability+Term), Terms),
    sumlist([Base|Terms], Score1),
    ability_max(Ability, Max),
    Score is min(Score1, Max).
ability(Ability, Score) :-
    ability_after_feats(Ability, Score).

% Ability score from base ability, race, abis and feats.
% Not allowed to exceed 20.
%natural_ability(Ability, Score) :-
%    base_ability(Ability, Base),
%    findall(Term, racial_abi(Ability+Term), Racial),
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

ability_mod(Abil, Mod) :-
    ability(Abil, Val),
    mf(Val, Mod).

% Size, speed, ...
size(Size) :-
    race(Race),
    racial_size(Race, Size).

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

% Spellcasting.
spell_save_dc(DC) :-
    trait(spellcasting(Abil)),
    ability_mod(Abil, Mod),
    proficiency_bonus(Bonus),
    DC is 8 + Bonus + Mod.

spell_attack_modifier(Mod) :-
    trait(spellcasting(Abil)),
    ability_mod(Abil, AbilMod),
    proficiency_bonus(Bonus),
    Mod is Bonus + AbilMod.

spell_slots(ClassName, SpellLevel, Slots) :-
    gain_spell_slots(ClassName, SpellLevel, Gains),
    class(Class),
    Class =.. [ClassName, ClassLevel],
    findall(X, (member(X,Gains),X=<ClassLevel), Xs),
    length(Xs, Slots),
    Slots > 0.
