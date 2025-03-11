% TODO delete this test case
%trait(test, creature(imp)).

(creature(Creature) ?= Desc) :-
    creature_desc(Creature, Desc).

creature(
    owl,
    _{ ac: 11,
       hp: 1,
       hp_roll: 1 d 4 - 1,
       speeds: [walking: feet(5), flying: feet(60)],
       initiative: 1,
       type: beast,
       size: tiny,
       alignment: unaligned,
       abilities: _{
           str: 3,
           dex: 13,
           con: 8,
           int: 2,
           wis: 12,
           cha: 7
       },
       proficiency_bonus: 2,
       skills: [perception, stealth],
       senses: [darkvision(feet(120), 'passive perception'(11))],
       challenge: 0,
       traits: _{
           flyby: "The owl doesn't provoke opportunity attacks when it flies out of an enemy's reach.",
           'keen hearing and sight': "The owl has advantage on Wisdom (Perception) checks that rely on hearing or sight."
       },
       actions: _{
           talons: attack{
                       range: melee,
                       to_hit: 3,
                       damage_rolls: [damage(slashing, 1)]
                   }
       }
     }
).

creature(
    imp,
    _{ ac: 13,
       hp: 10,
       hp_roll: 3 d 4 + 3,
       speeds: [walking: feet(20), flying: feet(40)],
       initiative: 3,
       type: fiend,
       size: tiny,
       alignment: lawful / evil,
       abilities: _{
           str: 6,
           dex: 17,
           con: 13,
           int: 11,
           wis: 12,
           cha: 14
       },
       proficiency_bonus: 2,
       skills: [deception, insight, persuasion, stealth],
       resistances:
         [ cold,
           (unless(magical or silvered): [piercing, bludgeoning, slashing]) ],
       immunities: [ fire, poison ],
       condition_immunities: [poisoned],
       senses: [darkvision(feet(120)), 'passive perception'(11)],
       languages: [infernal, common],
       challenge: 1,
       traits: _{
         shapechanger: "Polymorph into rat (20 ft.), raven (20 ft., fly 60 ft.), or spider (20 ft., climb 20 ft.). Stats unchanged except speed. Gear unaffected. Reverts on death.",
         'devil\'s sight': "Magical darkness doesn't impede the imp's darkvision.",
         'magic resistance': "Adv. on saving throws against spells and other magical effects."
       },
       actions: _{
         'sting (bite in beast form)':
           attack{
               range: melee,
               to_hit: 5,
               damage_rolls: [damage(piercing, 1 d 4 + 3), damage(poison, 3 d 6)],
               notes: ["DC 11 CON ST; on success: half poison damage"]
           },
         invisibility: "Turns invisible until it attacks or concentration ends; equipment also invisible."
        }
     }).

% ------------------------------------------------------------------------------
% Helper predicates for formatting creature stat cards.

% TODO maybe I should look into just generating this as HTML instead of generating markdown which is then converted to HTML on the frontend.

creature_desc(Creature, Desc) :-
    creature(Creature, Dict),
    findall(Page, fmt(format_creature_dict(Dict), Page), Desc).

format_creature_dict(Dict) -->
    { creature_abilities_markdown_table(Dict, Abilities)
    },
    paragraphs(
        [ (emph(unwords([[Dict.size], [Dict.type]])), [", "], emph(format_alignment(Dict.alignment))),
          format_creature_overview_table(Dict),
          [Abilities],
          format_creature_skills(Dict.abilities, Dict.proficiency_bonus, Dict.get(skills, [])),
          format_creature_resistances(Dict.get(resistances, [])),
          format_creature_damage_immunities(Dict.get(immunities, [])),
          format_creature_optional_list("Condition immunities", Dict.get(condition_immunities, [])),
          format_creature_optional_list("Senses", Dict.get(senses, [])),
          format_creature_optional_list("Languages", Dict.get(languages, []))
        ]).
format_creature_dict(Dict) -->
    { dict_pairs(Dict.get(traits, _{}), _, Traits),
      dict_pairs(Dict.get(actions, _{}), _, Actions)
    },
    optional(({Traits \= []}, ["---\n### Traits\n"]), []),
    foreach(member(Name-Desc,Traits), format_paragraph(capitalize_atom_words(Name), [Desc]), ["\n\n"]),
    ["\n\n"],
    optional(({Actions \= []}, ["---\n### Actions\n"]), []),
    foreach(member(Name-Spec,Actions), format_paragraph(capitalize_atom_words(Name), format_creature_action(Spec)), ["\n\n"]).

format_paragraph(HeaderPhrase, ContentPhrase) -->
    emph((phrase(HeaderPhrase), [". "])),
    phrase(ContentPhrase).

format_alignment(A1 / A2) --> unwords([[A1], [A2]]).
format_alignment(unaligned) --> [unaligned].

format_creature_skills(_, _, []) --> [].
format_creature_skills(Abilities, ProfBon, Skills) -->
    emph(["Skills. "]),
    foreach(member(S, Skills), format_skill(Abilities,ProfBon,S), [", "]).

format_skill(Abilities, ProfBon, Skill) -->
    { skill_ability(Skill, Ability),
      mf(Abilities.get(Ability), AbiMod),
      Bonus is AbiMod + ProfBon,
      skill_shorthand(Skill, Shorthand)
    },
    [Shorthand], [". "], format_bonus(Bonus).

format_creature_resistances([]) --> {!}, [].
format_creature_resistances(Resistances) -->
    {member(_:_, Resistances) -> Sep = ["; "] ; Sep = [", "]},
    emph(["Resistances. "]),
    foreach(member(R,Resistances), format_resistance_or_immunity(R), Sep).

format_creature_damage_immunities([]) --> {!}, [].
format_creature_damage_immunities(Immunities) -->
    emph(["Immunities. "]),
    foreach(member(I,Immunities), format_resistance_or_immunity(I), [", "]).

format_creature_optional_list(Header, Terms) -->
    optional(
        ( {Terms \= []}, emph(([Header], [". "])), format_list(Terms) ),
        []
    ).

format_resistance_or_immunity(unless(Cond):SimpleResistances) -->
    {!},
    ["unless "], format_simple_boolean_expression(Cond), [": "],
    format_list(SimpleResistances).
format_resistance_or_immunity(Atom) --> {atom(Atom), !}, [Atom].

format_simple_boolean_expression(A or B) -->
    [A], [" or "], [B].

format_creature_hp_and_roll(HP, HPRoll) -->
    format_number(HP),
    [" ("],
    format_dice_sum(HPRoll),
    [")"].

format_creature_overview_table(Dict) -->
    format_markdown_table(
        [ ["HP"]                                 - format_creature_hp_and_roll(Dict.hp, Dict.hp_roll),
          ["AC"]                                 - [Dict.ac],
          ["Init"]                               - format_bonus(Dict.initiative),
          format_speed_modes_header(Dict.speeds) - format_speeds(Dict.speeds),
          ["PB"]                                 - format_bonus(Dict.proficiency_bonus),
          ["CR (XP)"]                            - format_cr_and_xp(Dict.challenge)
        ]
    ).

format_speed_modes_header([walking: _]) --> {!}, ["Speed"].
format_speed_modes_header(Speeds) -->
    { Speeds \= [walking: _],
      maplist(speed_mode_abbrev, Speeds, Abbrevs)
    },
    ["Spd ("], format_list(Abbrevs), [")"].

format_speeds([walking: Spd]) --> {!}, format_term(Spd).
format_speeds(TaggedSpeeds) -->
    { TaggedSpeeds \= [walking: _],
      findall(Spd, member(_: Spd, TaggedSpeeds), Speeds)
    },
    format_list_empty_as_dash(Speeds).

speed_mode_abbrev(walking:_, wlk) :- !.
speed_mode_abbrev(flying:_, fly) :- !.
speed_mode_abbrev(swimming:_, swm) :- !.
speed_mode_abbrev(climbing:_, clb) :- !.
speed_mode_abbrev(X:_, X) :- !.

creature_abilities_markdown_table(Dict, Md) :-
    findall([Score, Bonus],
            (member(Abi, [str,dex,con,int,wis,cha]),
             Score = Dict.abilities.get(Abi),
             mf(Score, Modifier),
             fmt(format_bonus(Modifier), Bonus)
            ),
            ScoresAndBonuses),
    flatten(ScoresAndBonuses, FmtArgs),
    format(
        string(Md),
"
| STR     | DEX     | CON     | INT     | WIS     | CHA     |
|---------|---------|---------|---------|---------|---------|
| ~w (~w) | ~w (~w) | ~w (~w) | ~w (~w) | ~w (~w) | ~w (~w) |
",
        FmtArgs).

format_creature_action(attack{
               range: Range,
               to_hit: ToHit,
               damage_rolls: DamageRolls,
               notes: Notes
              }) -->
    {!},
    [Range], ["; "], % TODO
    format_bonus(ToHit), [" to hit;"],
    format_damage(DamageRolls), ["; "],
    foreach(member(Note,Notes), [Note], [", "]).
format_creature_action(Desc) -->
    {string(Desc)},
    [Desc].

format_cr_and_xp(CR) -->
    {cr_to_xp(CR, XP)},
    format_number(CR), [" ("], format_number(XP), [")"].

cr_to_xp(0, 10).
cr_to_xp(1, 200).
cr_to_xp(2, 450).
cr_to_xp(3, 700).
cr_to_xp(4, 1100).
cr_to_xp(5, 1800).
cr_to_xp(6, 2300).
cr_to_xp(7, 2900).
cr_to_xp(8, 3900).
cr_to_xp(9, 5000).
cr_to_xp(10, 5900).
cr_to_xp(11, 7200).
cr_to_xp(12, 8400).
cr_to_xp(13, 10000).
cr_to_xp(14, 11500).
cr_to_xp(15, 13000).
cr_to_xp(16, 15000).
cr_to_xp(17, 18000).
cr_to_xp(18, 20000).
cr_to_xp(19, 22000).
cr_to_xp(20, 25000).
cr_to_xp(21, 33000).
cr_to_xp(22, 41000).
cr_to_xp(23, 50000).
cr_to_xp(24, 62000).
cr_to_xp(25, 75000).
cr_to_xp(26, 90000).
cr_to_xp(27, 105000).
cr_to_xp(28, 120000).
cr_to_xp(29, 135000).
cr_to_xp(30, 155000).
