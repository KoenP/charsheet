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
         [ cold(half),
           unless(magical or silvered):
             [piercing(half), bludgeoning(half), slashing(half)],
           fire(full),
           poison(full)
         ],
       condition_immunities: [poisoned],
       senses: [darkvision(feet(120)), 'passive perception'(11)],
       languages: [infernal, common],
       challenge: 1,
       traits: _{
         shapechanger: "The imp can use its action to polymorph into a beast form that resembles a rat (speed 20 ft.), a raven (20 ft., fly 60 ft.), or a spider (20 ft., climb 20 ft.), or back into its true form. Its statistics are the same in each form, except for the speed changes noted. Any equipment it is wearing or carrying isn't transformed. It reverts to its true form if it dies.",
         'devil\'s sight': "Magical darkness doesn't impede the imp's darkvision.",
         'magic resistance': "The imp has advantage on saving throws against spells and other magical effects."
       },
       actions: _{
         'sting (bite in beast form)':
           attack{
               range: melee,
               to_hit: 5,
               damage_rolls: [damage(piercing, 1 d 4 + 3), damage(poison, 3 d 6)],
               notes: ["DC 11 CON ST; on success: half poison damage"]
           },
         invisibility: " The imp magically turns invisible until it attacks, or until its concentration ends (as if concentrating on a spell). Any equipment the imp wears or carries is invisible with it."
        }
     }).

% ------------------------------------------------------------------------------
% Helper predicates for formatting creature stat cards.
creature_desc(Creature, Desc) :-
    creature(Creature, Dict),
    fmt(format_creature_dict(Dict), Desc).

format_creature_dict(Dict) -->
    { Algmnt1 / Algmnt2  = Dict.alignment,
      creature_overview_markdown_table(Dict, Overview),
      creature_abilities_markdown_table(Dict, Abilities)
    },
    unlines(
        [ (emph(unwords([[Dict.size], [Dict.type]])), [", "], emph(unwords([[Algmnt1], [Algmnt2]]))),
          [Overview],
          [Abilities],
          format_creature_skills(Dict.abilities, Dict.proficiency_bonus, Dict.skills),
          format_creature_resistances(Dict.resistances)
        ]
    ).

format_creature_skills(_, _, []) --> {!}, [].
format_creature_skills(Abilities, ProfBon, Skills) -->
    {findall(format_skill(Abilities,ProfBon,S), member(S, Skills), Phrases)},
    emph(["Skills"]), [" "], sep(", ", Phrases).

format_skill(Abilities, ProfBon, Skill) -->
    { skill_ability(Skill, Ability),
      mf(Abilities.get(Ability), AbiMod),
      Bonus is AbiMod + ProfBon,
      skill_shorthand(Skill, Shorthand)
    },
    [Shorthand], [". "], format_bonus(Bonus).

format_creature_resistances([]) --> {!}, [].
format_creature_resistances(Resistances) -->
    [TODO].

creature_overview_markdown_table(Dict, Md) :-
    fmt(format_speed_modes_header(Dict.speeds), SpeedModesHdr),
    fmt(format_speeds(Dict.speeds), SpeedsStr),
    cr_to_xp(Dict.challenge, XP),
    format(
        string(Md),
"
| HP      | AC | Init. | Spd.~w | Prof. Bon. | CR (XP)  |
|---------|----|-------|-------|------------|---------|
| ~w (~w) | ~w | ~w    | ~w    | ~w         | ~w (~w) |
",
        [ SpeedModesHdr,
          Dict.hp, Dict.hp_roll, Dict.ac, Dict.initiative,
          SpeedsStr, Dict.proficiency_bonus, Dict.challenge, XP
        ]

    ).

format_speed_modes_header([walking: _]) --> {!}, ["Speed"].
format_speed_modes_header(Speeds) -->
    { Speeds \= [walking: _],
      maplist(speed_mode_abbrev, Speeds, Abbrevs)
    },
    [" ("], format_list(Abbrevs), [")"].

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
