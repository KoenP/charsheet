creature(
    imp,
    _{ ac: 13,
       hp: 10,
       hp_roll: 3 d 4 + 3,
       speeds: [walking: feet(20), flying: feet(40)],
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
           nonmagical and unsilvered:
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
creature_abilities_to_markdown(Dict, Md) :-
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
