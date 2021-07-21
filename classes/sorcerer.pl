:- [sorcerer/spells].
:- [sorcerer/draconic_bloodline].

class_option(sorcerer).
caster(sorcerer, full).
hd_per_level(sorcerer, 1 d 6).
initial_class_base_hp(sorcerer, 6).
max_hp_per_level(sorcerer, 1 d 6).
class_saving_throw(sorcerer, con).
class_saving_throw(sorcerer, cha).
choose_subclass_level(sorcerer, 1).

spellcasting_ability(sorcerer, cha).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Generic features and options available on level 1.

class_trait(sorcerer:1, weapon(dagger)).
class_trait(sorcerer:1, weapon(dart)).
class_trait(sorcerer:1, weapon(sling)).
class_trait(sorcerer:1, weapon(quarterstaff)).
class_trait(sorcerer:1, weapon('light crossbow')).
class_trait(sorcerer:1, spellcasting_focus(arcane)).

class_trait_options(sorcerer:1, skill, 2 from [skill(arcana      ) ,
                                               skill(deception   ) ,
                                               skill(insight     ) ,
                                               skill(intimidation) ,
                                               skill(persuasion  ) ,
                                               skill(religion    ) ]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Generic levelup features.
class_trait(sorcerer:2, 'font of magic').
class_trait_options(sorcerer:3, metamagic, 2 from Metamagic) :-
    Metamagic = [metamagic('careful spell'   ),
                 metamagic('distand spell'   ),
                 metamagic('empowered spell' ),
                 metamagic('extended spell'  ),
                 metamagic('heightened spell'),
                 metamagic('quickened spell' ),
                 metamagic('subtle spell'    ),
                 metamagic('twinned spell'   )].
class_trait(sorcerer:20, 'sorcerous restoration').

max_sorcery_points(Level) :-
    class_level(sorcerer:Level),
    Level > 1.
resource('sorcery points', SP) :-
    max_sorcery_points(SP).
on_long_rest('sorcery points', restore) :-
    trait('font of magic').
on_short_rest('sorcery points', restore(4)) :-
    trait('sorcerous restoration').

metamagic('careful spell') ?=
  "When you cast a spell that forces other creatures to make a saving throw, you can protect some of those creatures from the spell's full force. To do so, you spend 1 sorcery point and choose a number of those creatures up to your Charisma modifier (minimum of one creature). A chosen creature automatically succeeds on its saving throw against the spell.".
metamagic('distant spell') ?=
  "When you cast a spell that has a range of 5 feet or greater, you can spend 1 sorcery point to double the range of the spell.
When you cast a spell that has a range of touch, you can spend 1 sorcery point to make the range of the spell 30 feet.".
metamagic('empowered spell') ?=
  "When you roll damage for a spell, you can spend 1 sorcery point to reroll a number of the damage dice up to your Charisma modifier (minimum of one). You must use the new rolls.
You can use Empowered Spell even if you have already used a different Metamagic option during the casting of the spell.".
metamagic('extended spell') ?=
  "When you cast a spell that has a duration of 1 minute or longer, you can spend 1 sorcery point to double its duration, to a maximum duration of 24 hours.".
metamagic('heightened spell') ?=
  "When you cast a spell that forces a creature to make a saving throw to resist its effects, you can spend 3 sorcery points to give one target of the spell disadvantage on its first saving throw made against the spell.".
metamagic('quickened spell') ?=
  "When you cast a spell that has a casting time of 1 action, you can spend 2 sorcery points to change the casting time to 1 bonus action for this casting.".
metamagic('subtle spell') ?=
  "When you cast a spell, you can spend 1 sorcery point to cast it without any somatic or verbal components.".
metamagic('twinned spell') ?=
  "When you cast a spell that targets only one creature and doesn't have a range of self, you can spend a number of sorcery points equal to the spell's level to target a second creature in range with the same spell (1 sorcery point if the spell is a cantrip).
To be eligible, a spell must be incapable of targeting more than one creature at the spell's current level. For example, magic missile and scorching ray aren't eligible, but ray of frost and chromatic orb are. ".
custom_display_rule(metamagic(X), Str) :-
    atomic_list_concat(["metamagic (", X, ")"], Str).

'font of magic' ?= "At 2nd level, you tap into a deep wellspring of magic within yourself. This wellspring is represented by sorcery points, which allow you to create a variety of magical effects.
Sorcery Points

You have 2 sorcery points, and you gain more as you reach higher levels, as shown in the Sorcery Points column of the Sorcerer table. You can never have more sorcery points than shown on the table for your level. You regain all spent sorcery points when you finish a long rest.
Flexible Casting

You can use your sorcery points to gain additional spell slots, or sacrifice spell slots to gain additional sorcery points. You learn other ways to use your sorcery points as you reach higher levels.
Creating Spell Slots. You can transform unexpended sorcery points into one spell slot as a bonus action on your turn. The Creating Spell Slots table shows the cost of creating a spell slot of a given level. You can create spell slots no higher in level than 5th.
Any spell slot you create with this feature vanishes when you finish a long rest.
Spell Points
1st      2
2nd      3
3rd      5
4th      6
5th      7

Converting a Spell Slot to Sorcery Points. As a bonus action on your turn, you can expend one spell slot and gain a number of sorcery points equal to the slot's level.".

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Spellcasting.

% Cantrips.
class_trait_options(sorcerer:1, cantrip, 4 from Cantrips) :-
    learn_cantrip_options(sorcerer, Cantrips).
class_trait_options(sorcerer:Level, cantrip, 1 from Cantrips) :-
    member(Level, [4, 10]),
    learn_cantrip_options(sorcerer, Cantrips).
    

% Learning new spells.
sorcerer_proper_spell_known_at_level(Level, Spell) :-
    chosen_trait(class(sorcerer:LearnLevel), spell, learn_sorcerer_spell(Spell)),
    LearnLevel1 is LearnLevel + 1, Level1 is Level - 1,
    \+ (between(LearnLevel1, Level1, ForgetLevel),
        chosen_trait(class(sorcerer:ForgetLevel), replace_spell, replace_sorcerer_spell(Spell))).

spell_known(Spell, sorcerer, cha, always_available, at_will) :-
    trait(learn_spell(sorcerer, Spell)),
    cantrip(Spell).
spell_known(Spell, sorcerer, cha, always_available, spell_slot) :-
    trait(learn_sorcerer_spell(Spell)).

class_trait_options(sorcerer:1, spell, 2 from Spells) :-
    learn_sorcerer_spell_options(Spells).
class_trait_options(sorcerer:Level, spell, 1 from Spells) :-
    member(Level, [2,3,4,5,6,7,8,9,10,11,13,15,17]),
    learn_sorcerer_spell_options(Spells).

class_trait_options(sorcerer:Level, replace_spell, 2 from [1 from Forget, 1 from New]) :-
    between(2, 20, Level),
    Level1 is Level - 1,
    findall(forget_sorcerer_spell(Spell),
            sorcerer_proper_spell_known_at_level(Level1, Spell),
            Forget),
    learn_sorcerer_spell_options(New).
    

learn_sorcerer_spell_options(Spells) :-
    findall(learn_sorcerer_spell(Spell),
            (spell_learnable(sorcerer, Spell), spell(Spell, level, N), N > 0),
            Spells).
