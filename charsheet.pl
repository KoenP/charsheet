:- multifile
    describe/2,
    feature/2,
    feature_options/4,
    problem/2,
    todo/1.

:- [dice].
:- [classes].
:- [races].
:- [feats].
:- [skills].
:- [spells].
:- [abi].
:- [options].
:- [items].
:- [shortcuts].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Features.
matched(class(Class)) :-
    class(Class).
matched(class(Class,ClassLevel1)) :-
    class(Class, ClassLevel2),
    between(1, ClassLevel2, ClassLevel1).
matched(race(Race)) :-
    race(Race).
matched(feat(Feat)) :-
    valid_pick_feat(_, Feat).

feature(Feature) :-
    feature(Condition, Feature),
    matched(Condition).
feature(Feature) :-
    valid_pick(_, _, Feature).
feature(add_ability(Abil,Val)) :-
    valid_increase_ability_score(_, Abil, Val).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Character sheet calculations.

% Hit points.
max_hp(HP) :-
    findall(Term, hp_term_for_level(_, Term), Terms),
    sumlist(Terms, HP).

hp_term_for_level(1, Term) :-
    initial_class(Class),
    max_hp_initial(Class, Init),
    ability_mod(con, ConMod),
    Term is Init + ConMod.
hp_term_for_level(CharLevel, Term) :-
    gain_level(CharLevel, Class, HPMode),
    max_hp_per_level(Class, HPDie),
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
ability(Abil,Val) :-
    base_ability(Abil, Base),
    findall(Term, feature(add_ability(Abil,Term)), Terms),
    sumlist([Base | Terms], Val).

mf(Val, Mod) :-
    floor( (Val-10) / 2, Mod ).

ability_mod(Abil, Mod) :-
    ability(Abil, Val),
    mf(Val, Mod).

% Armor class.
ac(AC) :- feature(natural_armor(AC)), !.
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
    findall(Term, feature(add_initiative(Term)), Terms),
    sumlist([Mod | Terms], Init).

% Proficiency bonus.
calc_bonus(Level, Bonus) :- Bonus is 2 + div(Level-1, 4).
proficiency_bonus(Bonus) :- level(Level), calc_bonus(Level, Bonus).

% Spellcasting.
spell_save_dc(DC) :-
    feature(spellcasting(Abil)),
    ability_mod(Abil, Mod),
    proficiency_bonus(Bonus),
    DC is 8 + Bonus + Mod.

spell_attack_modifier(Mod) :-
    feature(spellcasting(Abil)),
    ability_mod(Abil, AbilMod),
    proficiency_bonus(Bonus),
    Mod is Bonus + AbilMod.

spell_slots(Class, SpellLevel, Slots) :-
    gain_spell_slots(Class, SpellLevel, Gains),
    class(Class, ClassLevel),
    findall(X, (member(X,Gains),X=<ClassLevel), Xs),
    length(Xs, Slots),
    Slots > 0.

% Levels.
level(Level) :-
    findall(L, gain_level(L,_,_), Levels),
    max_member(Level, Levels).

class(Class) :-
    initial_class(Class).
class(Class) :-
    findall(C, gain_level(_,C,_), Classes),
    list_to_set(Classes, ClassesSet),
    member(Class, ClassesSet).
class(Class, ClassLevel) :-
    level(CharLevel),
    class_option(Class),
    findall(L, (between(2,CharLevel,L),gain_level(L,Class,_)), Levels),
    (initial_class(Class) -> X = 1 ; X = 0),
    length(Levels, Len),
    ClassLevel is Len + X,
    ClassLevel > 0.

feat(Feat) :-
    matched(feat(Feat)).
