:- multifile
       % Indicates problems with your character sheet.
       problem/2,

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

       % resource(Name, Max)
       % Your PC's class-specific finite resources such as sorcery points,
       % wild shape uses, ...
       resource/2,

       % Register an effect to happen on long (/short) rest (usually resources being replenished).
       % on_long_rest(_, restore) means to restore a resource to max (0 used).
       % on_long_rest(_, restore(4)) means to restore a resource by 4.
       % on_long_rest(_, set(4)) means to set a resource to 4.
       on_long_rest/2,
       on_short_rest/2,

       % Refer to source material.
       source/2,

       trait_effect/2,

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

:- use_module(library(list_util)).
:- [util].
:- [dice].
:- [format].
:- [traits].
:- [options].
:- [spellcasting].
:- [class].
:- [race].
:- [background].
:- [feats].
:- [skills].
:- [leveling].
:- [items].
:- [shorthands].
:- [html].
:- [fishy].

%traits_at_level(Level, Traits) :-
%    between(1, 20, Level),
%    findall(Trait, gain_trait(Level, Trait), NewTraits),
%    findall(Trait, lose_trait(Level, Trait), LostTraits),
%    PrevLevel is Level - 1,
%    traits_at_level(PrevLevel, OldTraits)
%    append(NewTraits, OldTraits, )
    

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Character sheet calculations.

% Hit points.
max_hp(HP) :-
    findall(Term, (hp_term_for_level(_, Term); trait(max_hp + Term)), Terms),
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
resource(hit_dice(X), N) :-
    hit_dice(_, N d X).
custom_format(hit_dice(X)) --> {X \= _ d _}, ["hit dice (d"], [X], [")"].
on_rest(short, 'hit dice heal').
on_rest(long, regain('hit dice', M)) :-
    findall(N, resource(hit_dice(_), N), Ns),
    sumlist(Ns, SumNs),
    M is max(1, floor(SumNs / 2)).
custom_format(regain(R,N)) --> [R], [": regain up to "], [N].
'hit dice heal' ?= "A character can spend one or more Hit Dice at the end of a short rest, up to the character's maximum number of Hit Dice, which is equal to the character's level. For each Hit Die spent in this way, the player rolls the die and adds the character's Constitution modifier to it. The character regains hit points equal to the total. The player can decide to spend an additional Hit Die after each roll.".

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
:- table
   naked_lvl1_ability/2,
   ability_after_levelup_asis/3,
   ability_after_feats/2.

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
ac(AC) :- trait(natural_resilience(ArmorAC)), !,
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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- multifile
       name/1,
       race/1,
       initial_class/1,
       background/1,
       choose_traits/3,
       choose_subclass/2,
       equipped/1,
       gain_level/3.
       

name(_) :- false.
race(_) :- false.
initial_class(_) :- false.
background(_) :- false.
choose_trait(_,_,_) :- false.
choose_traits(_,_,_) :- false.
choose_subclass(_,_) :- false.
equipped(_) :- false.
gain_level(_,_,_) :- false.
