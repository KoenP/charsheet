/*
SESSIE 1
%%%%%%%%
the companion vernietigde de stad Eltorel

naar baldur's gate gevlucht

rhurad khada -> Shark god: Sekolah
Contract: dead three (bane, bhaal, myrkel (?)) -> cult
  Maken vd paniek gebruik om moorden te plegen die niet worden vervolgd.
  Zoek hun lair en los het op.

Hellriders
Sleep safe under companion's light
bound by mortal covenant, 


SESSIE 2
%%%%%%%%
Cult badhuis -> niks gevonden
Veel lichamen in park gevonden

Gevecht in park -> cultists met symbool van Bane

1 inspiration -> used


SESSIE 3
%%%%%%%%
PCs
- Sir Bashook Barcadioth (Tristan)
- Baba Anoush (swtst)
- Milo Greenbottle (andere Koen)
- Brom Pot (Kirsten)
- Pia Deepclaw (Femke)

We beginnen met short rest in tavern (Elfsong tavern).
Drinken rare groene brandy; gelatinous cube's acid brandy.

Hebben ons binnen gebullshit als Zodj (of hoe zijn naam ook alweer gespeld wordt).
Hebben geheime deur gevonden.
Vies water met kak in :( en nog koud ook

3 deuren
- W: buff dude met schedel als hoofd (bhaal)
- Z: handen van een skelet, houdt schreeuwende schedel vast (myrkel)
- O: lange man gekleed in armor met bucket helm; gauntlet aan rechterhand is zwart geverfd en heeft kettingen vast (bane)

Battle voorbij zuidelijke deur.
3 spellbooks in sarcofaag gevonden.

Bane deur: battle met twee armored priestachtige types
We redden de irritante nobleman.


*/

/*
# HIT POINTS
19 / 19

# HIT DICE (d8)
[ ] [ ]

# SORCERY POINTS
[ ] [ ]

# SPELL SLOTS
[ ] [ ]

# INVENTORY

- keyring cultist (7 keys)
*/

has('studded leather').

name("Alexander Sunstone").

base_ability(str,9).
base_ability(dex,13).
base_ability(con,15).
base_ability(int,11).
base_ability(wis,9).
base_ability(cha,16).

choice(init, 'base race', human).
choice(race(human), language, elvish).
choice(race(human), subrace, variant).
choice(race(human(variant)), asi, [con+1, dex+1]).
choice(race(human(variant)),skill,persuasion).
choice(race(human(variant)), feat,
       metamagic_adept('careful spell', 'empowered spell')).

choice(init, 'initial class', warlock).
choice(warlock >: 1, subclass, fiend).

choice(init, background, sage).
%choice(background(sage), language, [gnomish, infernal]).
trait(background(sage), language(gnomish)).
trait(background(sage), language(infernal)).

choice(warlock >: 1, cantrip, ['eldritch blast','minor illusion']).



choice(warlock >: 1,
       spell,
       [hex, 'burning hands']).
choice(^warlock, skill, [deception,intimidation]).

% Variant human with feat: metamagic

%choice(race(human),language,[dwarvish,elvish,giant,gnomish,goblin,halfling,orc,language,abyssal,celestial,draconic,'deep speech',infernal,primordial,sylvan,undercommon]).
%choice(race(human(variant)),skill,[athletics,acrobatics,'sleight of hand',stealth,arcana,history,investigation,nature,'animal handling',medicine,perception,survival,deception,intimidation,performance,persuasion]).
%choice(race(human(variant)),feat,[alert,durable,lucky]).
%choice(background(acolyte),language,[dwarvish,elvish,giant,gnomish,goblin,halfling,orc,language,abyssal,celestial,draconic,'deep speech',infernal,primordial,sylvan,undercommon]).
%choice(race(human(variant)),asi,2 from [str+1,dex+1,con+1,wis+1,int+1,cha+1]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
gain_level(2, warlock, hp_avg).

choice(warlock >: 2,
       'eldritch invocation',
       ['agonizing blast','mask of many faces']).

choice(warlock >: 2, spell, 'charm person').



spell_auto_data(hex, properties{ level: 1,
                                 higher_level: "",          
                                 school: enchantment,
                                 components: [v,s,m("the petrified eye of a newt")],
                                 range: feet(90),
                                 casting_time: "1 bonus action",
                                 duration: "1 hour",
                                 concentration: true,
                                 ritual: false,
                                 desc: "You place a curse on a creature that you can see within range. Until the spell ends, you deal an extra 1d6 necrotic damage to the target whenever you hit it with an attack. Also, choose one ability when you cast the spell. The target has disadvantage on ability checks made with the chosen ability.

If the target drops to 0 hit points before this spell ends, you can use a bonus action on a subsequent turn of yours to curse a new creature.

A Remove Curse cast on the target ends this spell early.

At Higher Levels. When you cast this spell using a spell slot of 3rd or 4th level, you can maintain your concentration on the spell for up to 8 hours. When you use a spell slot of 5th level or higher, you can maintain your concentration on the spell for up to 24 hours.",
                                 classes: [warlock],
                                 damage_with_cantrip_scaling: false,
                                 damage_at_slot_level: false,
                                 area_of_effect: false,
                                 dc: false,
                                 attack_type: false}).

feat_option(metamagic_adept(_,_)).
resource(metamagic, 'sorcery points', 2) :-
    trait(feat(metamagic_adept(_,_))).
on_rest(long, metamagic, full_restore).

feat(metamagic_adept(_,_)) ?= "You've learned how to exert your will on your spells to alter how they function:

    You learn two Metamagic options of your choice from the sorcerer class. You can use only one Metamagic option on a spell when you cast it, unless the option says otherwise. Whenever you reach a level that grants the Ability Score Improvement feature, you can replace one of these Metamagic options with another one from the sorcerer class.

    You gain 2 sorcery points to spend on Metamagic (these points are added to any sorcery points you have from another source but can be used only on Metamagic). You regain all spent sorcery points when you finish a long rest.

    Careful Spell. When you cast a spell that forces other creatures to make a saving throw, you can protect some of those creatures from the spell's full force. To do so, you spend 1 sorcery point and choose a number of those creatures up to your Charisma modifier (minimum of one creature). A chosen creature automatically succeeds on its saving throw against the spell.

    Distant Spell. When you cast a spell that has a range of 5 feet or greater, you can spend 1 sorcery point to double the range of the spell.
        When you cast a spell that has a range of touch, you can spend 1 sorcery point to make the range of the spell 30 feet.

    Empowered Spell. When you roll damage for a spell, you can spend 1 sorcery point to reroll a number of the damage dice up to your Charisma modifier (minimum of one). You must use the new rolls.
        You can use Empowered Spell even if you have already used a different Metamagic option during the casting of the spell.

    Extended Spell. When you cast a spell that has a duration of 1 minute or longer, you can spend 1 sorcery point to double its duration, to a maximum duration of 24 hours.

    Heightened Spell. When you cast a spell that forces a creature to make a saving throw to resist its effects, you can spend 3 sorcery points to give one target of the spell disadvantage on its first saving throw made against the spell.

    Quickened Spell. When you cast a spell that has a casting time of 1 action, you can spend 2 sorcery points to change the casting time to 1 bonus action for this casting.

    Seeking Spell. If you make an attack roll for a spell and miss, you can spend 2 sorcerer points to reroll the d20, and you must use the new roll.
        You can use Seeking Spell even if you have already used a different Metamagic option during the casting of the spell.

    Seeking Spell (UA). When you cast a spell that requires you to make a spell attack roll or that forces a target to make a Dexterity saving throw, you can spend 1 sorcery point to ignore the effects of half- and three-quarters cover against targets of the spell.

    Subtle Spell. When you cast a spell, you can spend 1 sorcery point to cast it without any somatic or verbal components.

    Transmuted Spell. When you cast a spell that deals a type of damage from the following list, you can spend 1 sorcery point to change that damage type to one of the other listed types: acid, cold, fire, lightning, poison, thunder.

    Twinned Spell. When you cast a spell that targets only one creature and doesn't have a range of self, you can spend a number of sorcery points equal to the spell's level to target a second creature in range with the same spell (1 sorcery point if the spell is a cantrip). To be eligible, a spell must be incapable of targeting more than one creature at the spell's current level. For example, magic missile and scorching ray aren't eligible, but ray of frost and chromatic orb are.".

