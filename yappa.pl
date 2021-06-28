:- discontiguous
       gain_level/3,
       spell/2,
       spell/3,
       describe/2,
       feature/2,
       feature_options/4,
       class_option/1,
       problem/2,
       todo/1.
:- op(500, xfx, d).

die_avg(X d Y, Avg) :- Avg is ceiling(X * (Y+1) / 2).

sum([], 0).
sum([X|Xs], Sum) :- sum(Xs, SumXs), Sum is X + SumXs.

class_option(fighter).
class_option(paladin).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Feats.
feature(feat(alert), add_initiative(5)).

% TODO denken over: feature(F), describe(F,D)
describe(feat(alert), "Always on the lookout for danger, you gain the following benefits:
    - You can’t be surprised while you are conscious.
    - You gain a +5 bonus to initiative.
    - Other creatures don’t gain advantage on attack rolls against you as a result of being hidden from you.
").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Spells.

spell(Name, class, Class) :-
    spell(Name, Properties),
    term_field(Properties, classes, Classes),
    member(Class, Classes).
spell(Name, component, Component) :-
    spell(Name, Properties),
    term_field(Properties, components, Components),
    member(Component, Components).
spell(Name, Prop, Value) :-
    spell(Name, Properties),
    term_field(Properties, Prop, Value).

term_field(Term, Field, Value) :-
    Term =.. L,
    member(Field:Value, L).

% Spell list.
spell(shillelagh,
      properties(
          school: transmutation,
          level: 0,
          classes: [druid],
          casting_time: bonus,
          range: touch,
          components: [v, s, m("mistletoe, a shamrock leaf, and a club or quarterstaff")],
          duration: (minutes(1)))).
describe(spell(shillelagh), "The wood of a club or quarterstaff you are holding is imbued with nature's power. For the duration, you can use your spellcasting ability instead of Strength for the attack and damage rolls of melee attacks using that weapon, and the weapon's damage die becomes a d8. The weapon also becomes magical, if it isn't already. The spell ends if you cast it again or if you let go of the weapon.").

spell(guidance,
      properties(
          school: divination,
          level: 0,
          classes: [druid, cleric], % TODO
          casting_time: action,
          range: touch,
          components: [v, s],
          duration: concentration(minutes(1)))).
describe(spell(guidance), "You touch one willing creature. Once before the spell ends, the target can roll a d4 and add the number rolled to one ability check of its choice. It can roll the die before or after making the ability check. The spell then ends.").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Tortle racial features.
feature(race(tortle), add_ability(str, 2)).
feature(race(tortle), add_ability(wis, 1)).
feature(race(tortle), size(medium)).
feature(race(tortle), claws). % todo
feature(race(tortle), hold_breath).
feature(race(tortle), natural_armor(17)).
feature(race(tortle), shell_defense).
feature(race(tortle), proficient(survival)).
feature(race(tortle), language(common)).
feature(race(tortle), language(aquan)).

describe(shell_defense, "You can withdraw into your shell as an action. Until you emerge, you gain a +4 bonus to AC, and you have advantage on Strength and Constitution saving throws. While in your shell, you are prone, your speed is 0 and can't increase, you have disadvantage on Dexterity saving throws, you can't take reactions, and the only action you can take is a bonus action to emerge from your shell.").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Druid class features.
class_option(druid).
hd_per_level(druid, 1 d 8).
max_hp_initial(druid, 8).
max_hp_per_level(druid, 1 d 8).

% hp TODO
% initial_hp(druid, )
saving_throw(druid, int).
saving_throw(druid, wis).

gain_spell_slots(druid, spell_level(1), [1,1,2,3]).
gain_spell_slots(druid, spell_level(2), [3,3,4]).
gain_spell_slots(druid, spell_level(3), [5,5,6]).
gain_spell_slots(druid, spell_level(4), [7,8,9]).
gain_spell_slots(druid, spell_level(5), [7,8,9]).
gain_spell_slots(druid, spell_level(6), [11,19]).
gain_spell_slots(druid, spell_level(7), [13,20]).
gain_spell_slots(druid, spell_level(8), [15]).
gain_spell_slots(druid, spell_level(9), [17]).

druid_cantrips(Cantrips) :- 
    findall(Cantrip, (spell(Cantrip, level, 0), spell(Cantrip, class, druid)), Cantrips).

% Features and options available on druid level 1.
feature(class(druid), proficient(light_armor  ) ).
feature(class(druid), proficient(medium_armor ) ).
feature(class(druid), proficient(shields      ) ).
feature(class(druid), proficient(clubs        ) ).
feature(class(druid), proficient(daggers      ) ).
feature(class(druid), proficient(darts        ) ).
feature(class(druid), proficient(javelins     ) ).
feature(class(druid), proficient(maces        ) ).
feature(class(druid), proficient(quarterstaffs) ).
feature(class(druid), proficient(scimitars    ) ).
feature(class(druid), proficient(sickles      ) ).
feature(class(druid), proficient(slings       ) ).
feature(class(druid), proficient(spears       ) ).
feature(class(druid), proficient(herbalism_kit) ).
feature(class(druid), language(druidic)).
feature(class(druid), spellcasting(wis)).
feature(class(druid), ritual_casting(druid)).
feature(class(druid), spellcasting_focus(druid)).
describe(language(druidic), "You know Druidic, the Secret language of druids. You can speak the language and use it to leave hidden messages. You and others who know this language automatically spot such a Message. Others spot the message's presence with a successful DC 15 Wisdom (Perception) check but can't decipher it without magic.").

feature_options(class(druid), druid_skills, 2, [ proficient(arcana)
                                               , proficient(animal_handling)
                                               , proficient(insight)
                                               , proficient(medicine)
                                               , proficient(nature)
                                               , proficient(perception)
                                               , proficient(religion)
                                               , proficient(survival)
                                               ]).

feature_options(class(druid), druid_cantrips, 2, Cantrips) :-
    druid_cantrips(Cantrips).

% Features and options unlocked by leveling up the druid class.
% TODO how to incorporate the wild shape rules.
feature(class(druid,2), wild_shape).
describe(wild_shape, "Starting at 2nd level, you can use your action to magically assume the shape of a beast that you have seen before. You can use this feature twice. You regain expended uses when you finish a short or long rest.
Your druid level determines the beasts you can transform into, as shown in Table: Beast Shapes. At 2nd level, for example, you can transform into any beast that has a challenge rating of 1/4 or lower that doesn’t have a flying or swimming speed.
You can stay in a beast shape for a number of hours equal to half your druid level (rounded down). You then revert to your normal form unless you expend another use of this feature. You can revert to your normal form earlier by using a bonus action on your turn. You automatically revert if you fall unconscious, drop to 0 hit points, or die.
While you are transformed, the following rules apply:").
describe(wild_shape, "Your game statistics are replaced by the statistics of the beast, but you retain your alignment, personality, and Intelligence, Wisdom, and Charisma scores. You also retain all of your skill and saving throw proficiencies, in addition to gaining those of the creature. If the creature has the same proficiency as you and the bonus in its stat block is higher than yours, use the creature’s bonus instead of yours. If the creature has any legendary or lair actions, you can’t use them.").
describe(wild_shape, "When you transform, you assume the beast’s hit points and Hit Dice. When you revert to your normal form, you return to the number of hit points you had before you transformed. However, if you revert as a result of dropping to 0 hit points, any excess damage carries over to your normal form. For example, if you take 10 damage in animal form and have only 1 hit point left, you revert and take 9 damage. As long as the excess damage doesn’t reduce your normal form to 0 hit points, you aren’t knocked unconscious.").
describe(wild_shape, "You can’t cast spells, and your ability to speak or take any action that requires hands is limited to the capabilities of your beast form. Transforming doesn’t break your concentration on a spell you’ve already cast, however, or prevent you from taking actions that are part of a spell, such as call lightning, that you’ve already cast.").
describe(wild_shape, "You retain the benefit of any features from your class, race, or other source and can use them if the new form is physically capable of doing so. However, you can’t use any of your special senses, such as darkvision, unless your new form also has that sense.").
describe(wild_shape, "You choose whether your equipment falls to the ground in your space, merges into your new form, or is worn by it. Worn equipment functions as normal, but the GM decides whether it is practical for the new form to wear a piece of equipment, based on the creature’s shape and size. Your equipment doesn’t change size or shape to match the new form, and any equipment that the new form can’t wear must either fall to the ground or merge with it. Equipment that merges with the form has no effect until you leave the form.").

%feature_options(class(druid,4), ability_score_improvement, 2, Options) :-
%    findall(add_ability(Ability,1), ability(Ability), Options).
feature_options(class(druid,4), druid_cantrips, 1, Cantrips) :-
    druid_cantrips(Cantrips).

    
% Logic for picking options. (TODO move)
problem(pick(Condition, Name), no_such_option) :-
    pick(Condition, Name, _),
    \+ feature_options(Condition, Name, _, _).
problem(pick(Condition, Name), condition_unmatched) :-
    pick(Condition, Name, _),
    \+ matched(Condition).
problem(pick(Condition, Name), (should_pick(NOptions), have_picked(NSelected))) :-
    pick(Condition, Name, Selection),
    feature_options(Condition, Name, NOptions, _),
    length(Selection, NSelected),
    NSelected \= NOptions.
problem(pick(Condition, Name), invalid_option(Pick)) :-
    pick(Condition, Name, Selection),
    feature_options(Condition, Name, _, Options),
    member(Pick, Selection),
    \+ member(Pick, Options).

todo(no_option_picked(Condition, Name)) :-
    feature_options(Condition, Name, _, _),
    matched(Condition),
    \+ pick(Condition, Name, _).
todo(unsolved_problems(Problem, Error)) :-
    problem(Problem, Error).

valid_pick(Condition, Name, Selection) :-
    pick(Condition, Name, Selection),
    \+ problem(pick(Condition, Name), _).

% A bit of a hack, perhaps.
valid_pick_at_level(CharLevel, Condition, Name, Selection) :-
    matched_at_level(CharLevel, Condition),
    pick(Condition, Name, Selection),
    problem(pick(Condition, Name), Reason) -> Reason = condition_unmatched.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Items.
item(padded_armor, value(5), weight(8)).

body_armor(padded, light, ac(11), [disadvantage(stealth)]).
body_armor(leather, light, ac(11), []).
body_armor(halfplate, medium, ac(15), [disadvantage(stealth)]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Features.
matched(Condition) :- level(CharLevel), matched_at_level(CharLevel, Condition).
matched_at_level(CharLevel, class(Class)) :-
    class_at_level(CharLevel, Class, _).
matched_at_level(CharLevel, class(Class,ClassLevel1)) :-
    class_at_level(CharLevel, Class, ClassLevel2),
    between(1, ClassLevel2, ClassLevel1).
matched_at_level(_, race(Race)) :-
    race(Race).
matched_at_level(CharLevel, feat(Feat)) :-
    pick_feat(Level, Feat),
    between(1, CharLevel, Level).

feature(Feature) :- level(CharLevel), feature_at_level(CharLevel, Feature).
feature_at_level(CharLevel, Feature) :-
    feature(Condition, Feature),
    matched_at_level(CharLevel, Condition).
feature_at_level(CharLevel, Feature) :-
    valid_pick_at_level(CharLevel, _, _, Feature).
feature_at_level(CharLevel, add_ability(Abil,Val)) :-
    valid_increase_ability_score_at_level(CharLevel, _, Abil, Val).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Options by leveling.
ability_score_increase_level(Level) :- member(Level, [4,8,12,16,19]).

valid_increase_ability_score(PickLevel, Abil, Val) :-
    level(CharLevel),
    valid_increase_ability_score_at_level(CharLevel, PickLevel, Abil, Val).
valid_increase_ability_score_at_level(CharLevel, PickLevel, Abil, Val) :-
    increase_ability_score(PickLevel, Abil, Val),
    PickLevel =< CharLevel,
    \+ problem(increase_ability_score(PickLevel, Abil, Val), _).

% Detect unspent ability score increases.
todo(ability_score_increase_unspent(level(Level))) :-
    ability_score_increase_level(Level),
    level(CharLevel),
    Level =< CharLevel,
    \+ (increase_ability_score(Level, _, _) ; pick_feat(Level, _)).

% Detect if we tried to add an abi on an inappropriate level.
problem(increase_ability_score(Level, Abil, Val), no_abi_level(Level)) :-
    increase_ability_score(Level, Abil, Val),
    \+ ability_score_increase_level(Level).

% Detect if we tried to add a feat on an inappropriate level.
problem(pick_feat(Level, Feat), no_abi_level(Level)) :-
    pick_feat(Level, Feat),
    \+ ability_score_increase_level(Level).

% Detect if ability score increases don't sum up to 2.
problem(increase_ability_score(Level, Abil, Val), not_equal_to_2) :-
    increase_ability_score(Level, Abil, Val),
    findall(Val, increase_ability_score(Level, _, Val), Vals),
    sum(Vals, SumVals),
    SumVals \= 2.

% Detect if ability score increases have been picked that the PC is
% not eligible for.
problem(increase_ability_score(Level, Abil, Val), level_too_low) :-
    increase_ability_score(Level, Abil, Val),
    level(CharLevel),
    CharLevel < Level.

% Detect if the player selected both a feat and an ability score increase.
problem(increase_ability_score(Level, Abil, Val), picked_both_feat_and_abi) :-
    problem(picked_both_feat_and_abi(Level,Abil,Val,_), picked_both_feat_and_abi).
problem(pick_feat(Level,Feat), picked_both_feat_and_abi) :-
    problem(picked_both_feat_and_abi(Level,_,_,Feat), picked_both_feat_and_abi).
problem(picked_both_feat_and_abi(Level,Abil,Val,Feat), picked_both_feat_and_abi) :-
    increase_ability_score(Level, Abil, Val),
    pick_feat(Level, Feat).


% TODO: Detect if ability score increase pushes the ability over 20.
% Should figure out what kind of things can affect ability scores though.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Character sheet calculations.

% Hit points.
max_hp(HP) :-
    findall(Term, hp_term_for_level(_, Term), Terms),
    sumlist(Terms, HP).

hp_term_for_level(1, Term) :-
    initial_class(Class),
    max_hp_initial(Class, Init),
    ability_at_level(1, con, Con),
    mf(Con, ConMod),
    Term is Init + ConMod.
hp_term_for_level(CharLevel, Term) :-
    gain_level(CharLevel, Class, HPMode),
    max_hp_per_level(Class, HPDie),
    hp_die(HPMode, HPDie, ClassHP),
    ability_at_level(1, con, Con),
    mf(Con, ConMod),
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
    level(CharLevel),
    ability_at_level(CharLevel, Abil, Val).
ability_at_level(CharLevel, Abil, Val) :-
    base_ability(Abil, Base),
    findall(Term, feature_at_level(CharLevel, add_ability(Abil,Term)), Terms),
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
    class_at_level(CharLevel, Class, ClassLevel).
class_at_level(CharLevel, Class, ClassLevel) :-
    class_option(Class),
    findall(L, (between(2,CharLevel,L),gain_level(L,Class,_)), Levels),
    (initial_class(Class) -> X = 1 ; X = 0),
    length(Levels, Len),
    ClassLevel is Len + X,
    ClassLevel > 0.

feat(Feat) :-
    matched(feat(Feat)).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Skills.
skill_ability(athletics       , str).
skill_ability(acrobatics      , dex).
skill_ability(sleight_of_hand , dex).
skill_ability(stealth         , dex).
skill_ability(arcana          , int).
skill_ability(history         , int).
skill_ability(investigation   , int).
skill_ability(nature          , int).
skill_ability(religion        , int).
skill_ability(animal_handling , wis).
skill_ability(insight         , wis).
skill_ability(medicine        , wis).
skill_ability(perception      , wis).
skill_ability(survival        , wis).
skill_ability(deception       , cha).
skill_ability(intimidation    , cha).
skill_ability(performance     , cha).
skill_ability(persuasion      , cha).

proficient(Skill) :- feature(proficient(Skill)).

skill_proficiency_bonus(Skill, Bonus) :-
    proficient(Skill), !,
    level(Level),
    calc_bonus(Level, Bonus).
skill_proficiency_bonus(Skill, 0) :- \+ proficient(Skill).

skill(Skill, Score) :-
    skill_ability(Skill, Abil),
    ability_mod(Abil, Mod),
    skill_proficiency_bonus(Skill, Bonus),
    Score is Mod + Bonus.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Character facts.

initial_class(druid).

gain_level(2, druid, hp_avg).

gain_level(3, druid, hp_avg).

gain_level(4, druid, hp_avg).
pick_feat(4, alert).
pick_feat(_,_) :- false.
increase_ability_score(_,_,_) :- false.
%increase_ability_score(3,dex,2).
%increase_ability_score(4,dex,2).

gain_level(5, druid, hp_avg).

gain_level(6, druid, hp_avg).


problem(gain_level, not_contiguous(Levels)) :-
    findall(L, gain_level(L,_,_), Levels),
    max_member(Highest, Levels),
    findall(L, between(2,Highest,L), Levels2),
    Levels \= Levels2.

pick(_,_,_) :- false.
pick(class(druid), druid_skills, [proficient(animal_handling), proficient(medicine)]).

race(_) :- false.
race(tortle).

base_ability(str, 10).
base_ability(dex, 8).
base_ability(con, 17).
base_ability(int, 16).
base_ability(wis, 18).
base_ability(cha, 8).

equipped(_) :- false.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% I/O
problems :-
    findall((P,E), problem(P,E), Problems),
    maplist(writeln, Problems).

todo :-
    findall(T, todo(T), Todos),
    maplist(writeln, Todos).

ac :- ac(AC), writeln(AC).

describe(X) :- describe(X,D), writeln(D).

match(X, Y) :-
    term_string(X, XString),
    string_to_list(XString, XList),
    term_string(Y, YString),
    string_to_list(YString, YList),
    append([_, XList, _], YList).
search(X) :-
    describe(Y, D),
    match(X, Y),
    write("Found: "),
    writeln(Y),
    writeln(D).
