%! skill(?Skill:atomic, ?Score:int)
%
%  Score is the bonus you get on rolls for Skill.
skill(Skill, Score) :-
    skill_ability(Skill, Ability),
    ability_mod(Ability, Mod),
    skill_proficiency_bonus(Skill, Bonus),
    Score is Mod + Bonus.

%! skill(?Skill:atomic)
skill(Skill) :- skill_ability(Skill, _).

%! skill_proficiency_bonus(?Skill:atomic, -Bonus:int)
skill_proficiency_bonus(Skill, Bonus) :-
    proficient_at_skill(Skill),
    level(Level),
    calc_bonus(Level, Bonus1),
    (trait(expertise(skill(Skill))) -> Bonus is 2*Bonus1 ; Bonus = Bonus1).
skill_proficiency_bonus(Skill, 0) :-
    \+ trait(skill(Skill)).

%! skill_ability(?Skill:atomic, ?Ability:atomic)
%
%  The Ability that covers Skill.
skill_ability(athletics         , str).
skill_ability(acrobatics        , dex).
skill_ability('sleight of hand' , dex).
skill_ability(stealth           , dex).
skill_ability(arcana            , int).
skill_ability(history           , int).
skill_ability(investigation     , int).
skill_ability(nature            , int).
skill_ability(religion          , int).
skill_ability('animal handling' , wis).
skill_ability(insight           , wis).
skill_ability(medicine          , wis).
skill_ability(perception        , wis).
skill_ability(survival          , wis).
skill_ability(deception         , cha).
skill_ability(intimidation      , cha).
skill_ability(performance       , cha).
skill_ability(persuasion        , cha).

%! skill_shorthand(?Skill:atomic, ?Shorthand:atomic)
%
%  An abbreviation for each skill.
skill_shorthand(athletics         , athl).
skill_shorthand(acrobatics        , acro).
skill_shorthand('sleight of hand' , 's. o. h').
skill_shorthand(stealth           , slth).
skill_shorthand(arcana            , arca).
skill_shorthand(history           , hist).
skill_shorthand(investigation     , invg).
skill_shorthand(nature            , nat).
skill_shorthand(religion          , reli).
skill_shorthand('animal handling' , 'a. h').
skill_shorthand(insight           , ins).
skill_shorthand(medicine          , mdc).
skill_shorthand(perception        , perc).
skill_shorthand(survival          , surv).
skill_shorthand(deception         , decp).
skill_shorthand(intimidation      , int).
skill_shorthand(performance       , perf).
skill_shorthand(persuasion        , pers).

%! proficient_at_skill(?Skill:atomic)
%
%  Checks whether character is proficient at given Skill.
%  When queried with a variable, each skill will only show up once.
proficient_at_skill(Skill) :-
    skill(Skill),
    proficient_at_skill_(Skill).
proficient_at_skill_(Skill) :-
    trait(skill(Skill)),
    !.

% The program should be able to deal with the same skill being
% selected more than once. Nevertheless, if the character has the same
% skill more than once and at least one of the times it is the result
% of a choice, we flag it as a problem.
problem(selected_same_skill_more_than_once(Skill)) :-
    Origin1 = choice(_,_),
    trait(Origin1, skill(Skill)),
    trait(Origin2, skill(Skill)),
    Origin1 \= Origin2.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

skill(athletics) ?= ["Your Strength (Athletics) check covers difficult situations you encounter while climbing, jumping, or swimming. Examples include the following activities:" ,
    "You attempt to climb a sheer or slippery cliff, avoid hazards while scaling a wall, or cling to a surface while something is trying to knock you off.",
    "You try to jump an unusually long distance or pull off a stunt midjump.",
    "You struggle to swim or stay afloat in treacherous currents, storm-tossed waves, or areas of thick seaweed. Or another creature tries to push or pull you underwater or otherwise interfere with your swimming."].


skill(acrobatics) ?= ["Your Dexterity (Acrobatics) check covers your attempt to stay on your feet in a tricky situation, such as when you're trying to run across a sheet of ice, balance on a tightrope, or stay upright on a rocking ship's deck. The GM might also call for a Dexterity (Acrobatics) check to see if you can perform acrobatic stunts, including dives, rolls, somersaults, and flips."].

'sleight of hand' ?= ["Whenever you attempt an act of legerdemain or manual trickery, such as planting something on someone else or concealing an object on your person, make a Dexterity (Sleight of Hand) check. The GM might also call for a Dexterity (Sleight of Hand) check to determine whether you can lift a coin purse off another person or slip something out of another person's pocket."].

skill(stealth) ?= ["Make a Dexterity (Stealth) check when you attempt to conceal yourself from enemies, slink past guards, slip away without being noticed, or sneak up on someone without being seen or heard."].

skill(arcana) ?= ["Your Intelligence (Arcana) check measures your ability to recall lore about spells, magic items, eldritch symbols, magical traditions, the planes of existence, and the inhabitants of those planes."].

skill(history) ?= ["Your Intelligence (History) check measures your ability to recall lore about historical events, legendary people, ancient kingdoms, past disputes, recent wars, and lost civilizations."].

skill(investigation) ?= ["When you look around for clues and make deductions based on those clues, you make an Intelligence (Investigation) check. You might deduce the location of a hidden object, discern from the appearance of a wound what kind of weapon dealt it, or determine the weakest point in a tunnel that could cause it to collapse. Poring through ancient scrolls in search of a hidden fragment of knowledge might also call for an Intelligence (Investigation) check."].

skill(nature) ?= ["Your Intelligence (Nature) check measures your ability to recall lore about terrain, plants and animals, the weather, and natural cycles."].

skill(religion) ?= ["Your Intelligence (Religion) check measures your ability to recall lore about deities, rites and prayers, religious hierarchies, holy symbols, and the practices of secret cults."].

skill('animal handling') ?= ["When there is any question whether you can calm down a domesticated animal, keep a mount from getting spooked, or intuit an animal’s intentions, the GM might call for a Wisdom (Animal Handling) check. You also make a Wisdom (Animal Handling) check to control your mount when you attempt a risky maneuver."].

skill(insight) ?= ["Your Wisdom (Insight) check decides whether you can determine the true intentions of a creature, such as when searching out a lie or predicting someone’s next move. Doing so involves gleaning clues from body language, speech habits, and changes in mannerisms."].

skill(medicine) ?= ["A Wisdom (Medicine) check lets you try to stabilize a dying companion or diagnose an illness."].

skill(perception) ?= ["Your Wisdom (Perception) check lets you spot, hear, or otherwise detect the presence of something. It measures your general awareness of your surroundings and the keenness of your senses. For example, you might try to hear a conversation through a closed door, eavesdrop under an open window, or hear monsters moving stealthily in the forest. Or you might try to spot things that are obscured or easy to miss, whether they are orcs lying in ambush on a road, thugs hiding in the shadows of an alley, or candlelight under a closed secret door."].

skill(survival) ?= ["The GM might ask you to make a Wisdom (Survival) check to follow tracks, hunt wild game, guide your group through frozen wastelands, identify signs that owlbears live nearby, predict the weather, or avoid quicksand and other natural hazards."].

skill(deception) ?= [
    "Your Charisma (Deception) check determines whether you can convincingly hide the truth, either verbally or through your actions. This deception can encompass everything from misleading others through ambiguity to telling outright lies. Typical situations include trying to fast-talk a guard, con a merchant, earn money through gambling, pass yourself off in a disguise, dull someone's suspicions with false assurances, or maintain a straight face while telling a blatant lie."
].

skill(intimidation) ?= ["When you attempt to influence someone through overt threats, hostile actions, and physical violence, the GM might ask you to make a Charisma (Intimidation) check. Examples include trying to pry information out of a prisoner, convincing street thugs to back down from a confrontation, or using the edge of a broken bottle to convince a sneering vizier to reconsider a decision."].

skill(performance) ?= ["Your Charisma (Performance) check determines how well you can delight an audience with music, dance, acting, storytelling, or some other form of entertainment."].

skill(persuasion) ?= ["When you attempt to influence someone or a group of people with tact, social graces, or good nature, the GM might ask you to make a Charisma (Persuasion) check. Typically, you use persuasion when acting in good faith, to foster friendships, make cordial requests, or exhibit proper etiquette. Examples of persuading others include convincing a chamberlain to let your party see the king, negotiating peace between warring tribes, or inspiring a crowd of townsfolk."].

(expertise(skill(Skill)) ?= Desc) :- (skill(Skill) ?= Desc).
