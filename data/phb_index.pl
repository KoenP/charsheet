% Player handbook index from https://github.com/copperdogma/dnd-phb-5e-index/blob/master/PHB%20Index%20Improved.json
% Plus some janky automation.

write_class_trait_index(Class) :-
    forall((index_trait_by_class(Class, Trait, Pages),
            map_matching_subterms(override_variable(New), Trait, Simplified)),
           write_term((Simplified @= phb(Pages)), [nl(true), quoted(true), fullstop=true,
                                                   variable_names(['_' = New])])).

override_variable(X, Y, X) :- var(Y).

format_feature_and_category(Category, Item) -->
    seq(Item), " (", seq(Category), ")".

find_phb_index(Category, Feature, PagesAtom) :-
    phb_index(IndexAtom, Pages),
    atom_codes(IndexAtom, IndexCodes),
    phrase(format_feature_and_category(CategoryCodes, FeatureCodes), IndexCodes),
    atom_codes(Category, CategoryCodes),
    atom_codes(Feature, FeatureCodes),
    term_to_atom(Pages, PagesAtom).

index_trait_by_class(Class, Trait, Pages) :-
    clause(trait_source(TraitSource, Trait), _),
    ground(TraitSource),
    (TraitSource = Class_ >: _ ; TraitSource = ^Class_),
    Class_ =.. [Class|_],
    simplify(Trait, SimplifiedTrait),
    find_phb_index(Class, SimplifiedTrait, Pages).

simplify(Atom, Atom) :- atom(Atom).
simplify(Compound, Simplified) :-
    Compound =.. [Base,_|_],
    atom_replace('_', ' ', Base, Simplified).

atom_replace(OldChar, NewChar, OldAtom, NewAtom) :-
    atom_codes(OldAtom, OldCodes),
    char_code(OldChar, OldCharCode),
    char_code(NewChar, NewCharCode),
    phrase(replace_char(OldCharCode, NewCharCode, OldCodes), NewCodes),
    atom_codes(NewAtom, NewCodes).

replace_char(Old, New, [Old|Rest]) --> [New], replace_char(Old, New, Rest).
replace_char(Old, New, [X|Rest]) --> {X \= Old}, [X], replace_char(Old, New, Rest).
replace_char(_, _, []) --> [].


phb_index('0 hit points (hit points: dropping to 0)', 197-198).
phb_index('ability check', 7).
phb_index('contest', 174).
phb_index('group', 175).
phb_index('passive', 175).
phb_index('skill', 174-175).
phb_index('skills with different abilities (variant)', 175).
phb_index('working together', 175).
phb_index('ability modifier', 7).
phb_index('determining', 13).
phb_index('table', 13).
phb_index('ability score', 7).
phb_index('customizing (variant)', 13).
phb_index('determining', 12-13).
phb_index('increase (racial traits)', 12).
phb_index('point cost table', 13).
phb_index('rolling', 13).
phb_index('standard set', 13).
phb_index('summary', 12).
phb_index('using', 173-179).
phb_index('ability score improvement. see specific class entries', 45).
phb_index('abjuration', 115).
phb_index('school (wizard)', 115-116).
phb_index('abjuration savant (wizard)', 115).
phb_index('abjure enemy (paladin)', 88).
phb_index('abjurer', 115).
phb_index('abyss, the (plane of existence)', 302).
phb_index('abyssal (language)', 123).
phb_index('ac (armor class)', 7).
phb_index('archeron (plane of existence)', 302).
phb_index('acid damage (damage type)', 196).
phb_index('acolyte (background)', 127).
phb_index('acolyte of nature (cleric)', 62).
phb_index('acrobatics (dexterity skill)', 176).
phb_index('action', 189).
phb_index('attack action', 192).
phb_index('cast a spell (casting a spell: casting time)', 202).
phb_index('dash action', 192).
phb_index('disengage action', 192).
phb_index('dodge action', 192).
phb_index('help action', 192).
phb_index('hide action', 192).
phb_index('improvising', 193).
phb_index('ready action', 193).
phb_index('search action', 193).
phb_index('use an object action', 193).
phb_index('action surge (fighter)', 72).
phb_index('advancement (character)', 15).
phb_index('advantage', 7).
phb_index('adventure', 5).
phb_index('adventuring', 181-187).
phb_index('adventurer (character)', 5).
phb_index('adventuring', 181-187).
phb_index('adventuring gear', 148).
phb_index('table', 150).
phb_index('age (character). see specific race entries', 17).
phb_index('agonizing blast (warlock eldritch invocation)', 110).
phb_index('alignment', 122).
phb_index('of planes', 302).
phb_index('alter memories (wizard)', 117).
phb_index('ammunition (weapon property)', 146).
phb_index('animal handling (wisdom skill)', 178).
phb_index('aquan (language)', 123).
phb_index('arborea (plane of existence)', 302).
phb_index('arcadia (plane of existence)', 302).
phb_index('arcana (intelligence skill)', 177).
phb_index('arcane charge (fighter)', 75).
phb_index('arcane focus', 151).
phb_index('sorcerer equipment/spellcasing focus', 101).
phb_index('warlock equipment/spellcasing focus', 107).
phb_index('wizard equipment/spellcasing focus', 114).
phb_index('material spellcasting component', 203).
phb_index('arcane magic', 205).
phb_index('bard', 51).
phb_index('fighter (eldritch knight martial archetype)', 74).
phb_index('rogue (arcane trickster roguish archetype)', 97).
phb_index('sorcerer', 99).
phb_index('warlock', 105).
phb_index('wizard', 112).
phb_index('arcane recovery (wizard)', 115).
phb_index('arcane tradition (wizard)', 115).
phb_index('arcane traditions', 115-119).
phb_index('school of abjuration', 115-116).
phb_index('school of conjuration', 116).
phb_index('school of divination', 116-117).
phb_index('school of enchantment', 117).
phb_index('school of evocation', 117-118).
phb_index('school of illusion', 118).
phb_index('school of necromancy', 118-119).
phb_index('school of transmutation', 119).
phb_index('arcane trickster (roguish archetype)', 97).
phb_index('arcane ward (wizard)', 115).
phb_index('archdruid (druid)', 67-68).
phb_index('archery (fighting style)', 72).
phb_index('archfey, the (warlock otherworldly patron)', 108-109).
phb_index('area of effect', 204-205).
phb_index('armor and shields', 144-146).
phb_index('barding', 155).
phb_index('casting a spell in armor', 201).
phb_index('getting into and out of', 146).
phb_index('refitting plate armor (variant: equipment sizes)', 144).
phb_index('stealth', 144).
phb_index('table', 145).
phb_index('armor class (ac)', 7).
phb_index('armor of shadows (warlock eldritch invocation)', 110).
phb_index('armor proficiency', 144).
phb_index('see also specific class entries', 45).
phb_index('artificer\'s lore (rock gnome)', 37).
phb_index('ascendant step (warlock eldritch invocation)', 110).
phb_index('aspect of the beast (barbarian)', 50).
phb_index('assassin (rogue)', 97).
phb_index('assassinate (rogue)', 97).
phb_index('astral plane (plane of existence)', 302).
phb_index('athletics (strength skill)', 175).
phb_index('attack of opportunity', 195).
phb_index('attack', 14).
phb_index('attack action', 192).
phb_index('attack modifier', 14).
phb_index('spell (attack roll)', 205).
phb_index('attack roll', 7).
phb_index('ability modifier', 194).
phb_index('dexterity-based', 177).
phb_index('modifiers to', 194).
phb_index('proficiency bonus', 194).
phb_index('rolling a 1 (automatic miss)', 194).
phb_index('rolling a 20 (automatic hit)', 194).
phb_index('spell (attack roll)', 205).
phb_index('strength-based', 176).
phb_index('auran (language)', 123).
phb_index('aura of courage (paladin)', 85).
phb_index('aura of devotion (paladin)', 86).
phb_index('aura of protection (paladin)', 85).
phb_index('aura of warding (paladin)', 87).
phb_index('automatic hit (rolling a 20)', 194).
phb_index('automatic miss (rolling a 1)', 194).
phb_index('avatar of battle (cleric)', 63).
phb_index('avenging angel (paladin)', 88).
phb_index('awakened mind (warlock)', 110).
phb_index('background', 11).
phb_index('acolyte', 127).
phb_index('charlatan', 128).
phb_index('criminal', 129-130).
phb_index('customizing', 125-126).
phb_index('entertainer', 130-131).
phb_index('equipment', 125).
phb_index('folk hero', 131-132).
phb_index('gladiator (variant)', 131).
phb_index('guild artisan', 132-133).
phb_index('guild merchant (variant)', 133).
phb_index('hermit', 134-135).
phb_index('languages', 125).
phb_index('noble', 135-136).
phb_index('noble knight (variant)', 136).
phb_index('outlander', 136-137).
phb_index('pirate (variant)', 139).
phb_index('proficiencies', 125).
phb_index('sage', 137-138).
phb_index('sailor', 139).
phb_index('soldier', 140-141).
phb_index('spy (variant)', 130).
phb_index('suggested characteristics', 125).
phb_index('urchin', 141).
phb_index('bad reputation (pirate variant feature)', 139).
phb_index('barbarian', 45).
phb_index('primal paths', 49-50).
phb_index('quick build', 47).
phb_index('bard', 45).
phb_index('colleges', 54-55).
phb_index('quick build', 52).
phb_index('spell list', 207).
phb_index('bard college (bard)', 54).
phb_index('bard colleges', 54-55).
phb_index('college of lore', 54-55).
phb_index('college of valor', 55).
phb_index('bardic inspiration (bard)', 53-54).
phb_index('barding', 155).
phb_index('base attack bonus (proficiency bonus)', 12).
phb_index('base save bonus (proficiency bonus)', 12).
phb_index('bat (creature)', 304).
phb_index('battle magic (bard)', 55).
phb_index('battle master (fighter martial archetype)', 73).
phb_index('bear, black (creature)', 304).
phb_index('bear, brown (creature)', 304).
phb_index('beastlands, the (plane of existence)', 302).
phb_index('beast master (ranger archetype)', 93).
phb_index('beast speech (warlock eldritch invocation)', 110).
phb_index('beast spells (druid)', 67).
phb_index('beguiling defenses (warlock)', 109).
phb_index('beguiling influence (warlock eldritch invocation)', 110).
phb_index('bend luck (sorcerer)', 103).
phb_index('benign transposition (wizard)', 116).
phb_index('bestial fury (ranger)', 93).
phb_index('bewitching whispers (warlock eldritch invocation)', 110).
phb_index('blessed healer (cleric)', 60).
phb_index('blessings of knowledge (cleric)', 59).
phb_index('blessing of the trickster (cleric)', 63).
phb_index('blinded (condition)', 290).
phb_index('blindsense (rogue)', 96).
phb_index('blindsight', 183).
phb_index('bludgeoning damage (damage type)', 196).
phb_index('boar (creature)', 304).
phb_index('bonds (personality)', 124).
phb_index('bonus', 7).
phb_index('bonus action', 189).
phb_index('see also casting a spell: casting time', 202).
phb_index('book of ancient secrets (warlock eldritch invocation)', 110).
phb_index('book of shadows (warlock)', 108).
phb_index('border ethereal (plane of existence)', 302).
phb_index('brave (halfling)', 28).
phb_index('breaking concentration', 203).
phb_index('breath of winter (monk elemental discipline)', 81).
phb_index('breath weapon (dragonborn)', 34).
phb_index('bright light', 183).
phb_index('bringing back the dead, see the spell descriptions for raise dead (270), reincarnate (271), resurrection (272), revivify (272), true resurrection (284)', null).
phb_index('brutal critical (barbarian)', 49).
phb_index('bull rush (shoving)', 195).
phb_index('by popular demand (entertainer)', 130).
phb_index('bytopia (plane of existence)', 302).
phb_index('calishite (human ethnicity)', 30).
phb_index('campaign', 5).
phb_index('cantrips', 201).
phb_index('see also spellcasting: bard (52); cleric (58); druid (66); fighter, eldritch knight (75); rogue, arcane trickster (98); sorcerer (101); warlock (107); wizard (114)', null).
phb_index('carceri (plane of existence)', 302).
phb_index('careful spell (sorcerer metamagic)', 102).
phb_index('carrying capacity (lifting and carrying)', 176).
phb_index('casting a spell', 201-205).
phb_index('area of effect', 204-205).
phb_index('at a higher level', 201).
phb_index('attack roll', 205).
phb_index('casting time', 202).
phb_index('combining effects', 205).
phb_index('components', 203).
phb_index('duration', 203-204).
phb_index('in armor', 201).
phb_index('multiclassing', 164).
phb_index('range', 202-203).
phb_index('saving throw', 205).
phb_index('targeting', 204).
phb_index('see also spellcasting: bard (52); cleric (58); druid (66); fighter, eldritch knight (75); ranger (91); rogue, arcane trickster (98); sorcerer (101); warlock (107); wizard (114)', null).
phb_index('casting time', 202).
phb_index('cat (creature)', 305).
phb_index('celestial (language)', 123).
phb_index('chains of carceri (warlock eldritch invocation)', 110).
phb_index('champion (fighter martial archetype)', 72).
phb_index('channel divinity, cleric', 58-63).
phb_index('charm animals and plants (nature domain)', 62).
phb_index('cloak of shadows (trickery domain)', 63).
phb_index('destroy undead', 59).
phb_index('destructive wrath (tempest domain)', 62).
phb_index('guided strike (war domain)', 63).
phb_index('invoke duplicity (trickery domain)', 63).
phb_index('knowledge of the ages (knowledge domain)', 59).
phb_index('multiclassing and', 164).
phb_index('preserve life (life domain)', 60).
phb_index('radiance of the dawn (light domain)', 61).
phb_index('read thoughts (knowledge domain)', 59-60).
phb_index('turn undead', 59).
phb_index('war god\'s blessing (war domain)', 63).
phb_index('channel divinity, paladin', 86-88).
phb_index('abjure enemy (oath of vengeance)', 88).
phb_index('nature’s wrath (oath of the ancients)', 87).
phb_index('sacred weapon (oath of devotion)', 86).
phb_index('turn the faithless (oath of the ancients)', 87).
phb_index('turn the unholy (oath of devotion)', 86).
phb_index('vow of enmity (oath of vengeance)', 88).
phb_index('chaotic evil (alignment)', 122).
phb_index('chaotic evil (alignment)', 122).
phb_index('chaotic good (alignment)', 122).
phb_index('chaotic neutral (alignment)', 122).
phb_index('character', 5).
phb_index('advancement', 15).
phb_index('age. see specific race entries', 17).
phb_index('alignment', 122).
phb_index('creating a', 11-15).
phb_index('describing your', 13-14).
phb_index('equipping your', 14).
phb_index('height and weight', 121).
phb_index('name', 121).
phb_index('personality', 122-124).
phb_index('sex and gender', 121).
phb_index('character sheet', 11).
phb_index('charisma', 12).
phb_index('checks', 178-179).
phb_index('deception', 178).
phb_index('intimidation', 179).
phb_index('performance', 179).
phb_index('persuasion', 179).
phb_index('charlatan (background)', 128).
phb_index('charm animals and plants (channel divinity cleric option)', 62).
phb_index('charmed (condition)', 290).
phb_index('check. see ability check', null).
phb_index('chondathan (human ethnicity)', 30).
phb_index('circle forms (druid)', 69).
phb_index('circle of the land (druid circle)', 68).
phb_index('circle of the moon (druid circle)', 69).
phb_index('circle spells (druid)', 68).
phb_index('city secrets (urchin)', 141).
phb_index('cleansing touch (paladin)', 85).
phb_index('cloak of shadows', null).
phb_index('channel divinity cleric options', 63).
phb_index('monk', 80).
phb_index('class', 11).
phb_index('choosing a', 11).
phb_index('features', 11).
phb_index('proficiencies', 12).
phb_index('quick build', 11).
phb_index('see also specific class entries', 45).
phb_index('clear path to the target (casting a spell: targeting)', 204).
phb_index('clench of the north wind (monk elemental discipline)', 81).
phb_index('cleric', 45).
phb_index('divine domains', 59).
phb_index('quick build', 57).
phb_index('spell list', 207-208).
phb_index('climbing (movement)', 182).
phb_index('coinage', 143).
phb_index('cold damage (damage type)', 196).
phb_index('college of lore (bard college)', 54).
phb_index('college of valor (bard college)', 55).
phb_index('colossus slayer (ranger hunter\'s prey)', 93).
phb_index('combat', 8).
phb_index('mounted', 198).
phb_index('step by step', 189).
phb_index('underwater', 198).
phb_index('combat inspiration (bard)', 55).
phb_index('combat round (time)', 181).
phb_index('combat superiority (fighter)', 73).
phb_index('combat wild shape (druid)', 69).
phb_index('combining spell effects (casting a spell)', 205).
phb_index('commander\'s strike maneuver (fighter maneuver)', 74).
phb_index('common (language)', 123).
phb_index('common races', 17).
phb_index('command undead (wizard)', 119).
phb_index('component, spell (casting a spell)', 203).
phb_index('material spell component', 203).
phb_index('somatic spell component', 203).
phb_index('verbal spell component', 203).
phb_index('concentration', 203-204).
phb_index('conditions', 290-292).
phb_index('cone (area of effect)', 204).
phb_index('conjuration', 116).
phb_index('school (wizard arcane tradition)', 116).
phb_index('conjuration savant (wizard)', 116).
phb_index('conjurer', 116).
phb_index('constitution', 12).
phb_index('checks', 177).
phb_index('hit points and', 177).
phb_index('container capacity', 153).
phb_index('contest (ability check)', 174).
phb_index('controlled chaos (sorcerer)', 103).
phb_index('converting a spell slot to sorcery points (sorcerer font of magic)', 101).
phb_index('copper piece (cp) (coinage)', 143).
phb_index('corona of light (cleric)', 61).
phb_index('countercharm (bard)', 54).
phb_index('cover', 196).
phb_index('crafting (downtime activity)', 187).
phb_index('crawling (movement)', 182).
phb_index('create thrall (warlock)', 110).
phb_index('creating a character', 11-15).
phb_index('creating spell slots (sorcerer font of magic)', 101).
phb_index('creature statistics', 304-311).
phb_index('criminal (background)', 129-130).
phb_index('criminal contact (criminal)', 129).
phb_index('critical hit', 197).
phb_index('crocodile (creature)', 305).
phb_index('cube (area of effect)', 204).
phb_index('cunning action (rogue)', 96).
phb_index('current hit points (current)', 196).
phb_index('cutting words (bard)', 54-55).
phb_index('cylinder (area effect)', 204).
phb_index('damage', 14).
phb_index('at o hit points (death saving throw)', 197).
phb_index('damage resistance', 97).
phb_index('damage resistance (dragonborn)', 34).
phb_index('damage roll', 14).
phb_index('dexterity-based', 177).
phb_index('spell', 196).
phb_index('strength-based', 76).
phb_index('more than one target', 196).
phb_index('weapon', 14).
phb_index('damage types', 196).
phb_index('damage vulnerability', 197).
phb_index('damaran (human ethnicity)', 31).
phb_index('dampen elements (cleric)', 62).
phb_index('danger sense (barbarian)', 48).
phb_index('dark delirium (warlock)', 109).
phb_index('dark elf', 24).
phb_index('darkness', 183).
phb_index('dark one\'s blessing (warlock)', 109).
phb_index('dark one\'s own luck (warlock)', 109).
phb_index('darkvision', 183).
phb_index('see also specific race entries', 45).
phb_index('dash action', 192).
phb_index('dc (difficulty class)', 7).
phb_index('dead', 197).
phb_index('bringing back the, see the spell descriptions for raise dead (270), reincarnate (271), resurrection (272), revivify (272), true resurrection (284)', null).
phb_index('deafened (condition)', 290).
phb_index('death', 197).
phb_index('instant', 197).
phb_index('monsters and', 198).
phb_index('death domain', 293).
phb_index('death saving throw', 197).
phb_index('damage at o hit points', 197).
phb_index('rolling a 1 or 20 on', 197).
phb_index('death strike (rogue)', 97).
phb_index('deception (charisma skill)', 178).
phb_index('deep gnome', 36).
phb_index('deep speech (language)', 123).
phb_index('defense (fighting style)', 72).
phb_index('defensive tactics (ranger)', 93).
phb_index('deflect missiles (monk)', 78).
phb_index('dehydration (food and drink: water requirements)', 185).
phb_index('deities', 293-299).
phb_index('celtic', 297).
phb_index('dragonlance', 293).
phb_index('eberron', 293).
phb_index('egyptian', 297-298).
phb_index('forgotten realms', 293).
phb_index('greek', 297).
phb_index('greyhawk', 293).
phb_index('nonhuman', 293).
phb_index('norse', 298).
phb_index('see also cleric (56); druid (64); paladin (82)', null).
phb_index('demiplanes (plane of existence)', 302).
phb_index('destroy undead (channel divinity cleric option)', 59).
phb_index('destructive wrath (channel divinity cleric option)', 62).
phb_index('devil\'s sight (warlock eldritch invocation)', 110).
phb_index('dexterity', 12).
phb_index('acrobatics', 176).
phb_index('checks', 176-177).
phb_index('sleight of hand', 177).
phb_index('stealth', 177).
phb_index('diamond soul (monk)', 79).
phb_index('d (die)', 6-7).
phb_index('dice', 6-7).
phb_index('d2 or d3', 7).
phb_index('percentile', 6).
phb_index('difficult terrain (movement)', 182).
phb_index('difficulty class (dc)', 7).
phb_index('typical dcs table', 174).
phb_index('dim light', 183).
phb_index('disadvantage', 7).
phb_index('disarming attack maneuver (fighter maneuver)', 74).
phb_index('disciple of life (cleric)', 60).
phb_index('disciple of the elements (monk)', 80).
phb_index('discovery (hermit)', 134).
phb_index('disengage action', 192).
phb_index('distant spell (sorcerer metamagic)', 102).
phb_index('distracting strike maneuver (fighter maneuver)', 74).
phb_index('divination', 116-117).
phb_index('school (wizard arcane tradition)', 116-117).
phb_index('divination savant (wizard)', 116).
phb_index('divine domain (cleric)', 58).
phb_index('domain spells', 58).
phb_index('divine domains', 59).
phb_index('knowledge', 59-60).
phb_index('life', 60).
phb_index('light', 60-61).
phb_index('nature', 61-62).
phb_index('tempest', 62).
phb_index('trickery', 62-63).
phb_index('war', 63).
phb_index('divine health (paladin)', 85).
phb_index('divine intervention (cleric)', 59).
phb_index('divine magic', 205).
phb_index('see also cleric (56); druid (64); paladin (82); ranger (89)', null).
phb_index('diviner', 116).
phb_index('divine sense (paladin)', 84).
phb_index('divine smite (paladin)', 85).
phb_index('divine strike (cleric)', null).
phb_index('life domain', 60).
phb_index('nature domain', 62).
phb_index('tempest domain', 62).
phb_index('trickery domain', 63).
phb_index('war domain', 63).
phb_index('dm (dungeon master)', 5).
phb_index('dodge action', 192).
phb_index('domain spells (divine domain)', 59).
phb_index('donning and doffing armor (armor and shields: getting into and out of)', 146).
phb_index('downtime activities', 187).
phb_index('draconians', 34).
phb_index('draconic', null).
phb_index('ancestry', 34).
phb_index('alphabet', 124).
phb_index('language', 17).
phb_index('draconic ancestry (dragonborn)', 24).
phb_index('draconic presence (sorcerer)', 102).
phb_index('draconic resilience (sorcerer)', 102).
phb_index('dragon ancestor (sorcerer)', 102).
phb_index('dragonborn', 32-34).
phb_index('dragonborn names', 33-34).
phb_index('dragonborn traits', 34).
phb_index('dragon wings (sorcerer)', 103).
phb_index('draw or sheathe a weapon (objects: using during combat)', 190).
phb_index('dreadful word (warlock eldritch invocation)', 110).
phb_index('drink (expenses)', 158).
phb_index('drop an object (objects: using during combat)', 190).
phb_index('drow (race; elf: dark elf)', 24).
phb_index('drow magic', 24).
phb_index('druid', 45).
phb_index('druid circles', 68).
phb_index('quick build', 65).
phb_index('spell list', 208).
phb_index('druid circle', 67).
phb_index('druid circles', 68-69).
phb_index('circle of the land', 68-69).
phb_index('circle of the moon', 69).
phb_index('druidic', 66).
phb_index('druidic focus', 150).
phb_index('spellcasting focus: druid', 66).
phb_index('druids and the gods', 69).
phb_index('dueling (fighting style)', 72).
phb_index('duergar (gray dwarf) (race; dwarf)', 20).
phb_index('dungeon master (dm)', 5).
phb_index('durable summons (wizard)', 116).
phb_index('duration (casting a spell)', 203-204).
phb_index('dwarf', 18-20).
phb_index('dwarf names', 20).
phb_index('dwarf traits', 20).
phb_index('dwarven armor training (mountain dwarf)', 20).
phb_index('dwarven combat training', 20).
phb_index('dwarven resilience', 20).
phb_index('dwarven toughness (hill dwarf)', 20).
phb_index('dwarvish', 20).
phb_index('alphabet', 122).
phb_index('language', 20).
phb_index('eagle, giant (creature)', 306).
phb_index('effect', 201).
phb_index('elder champion (paladin)', 87).
phb_index('eldritch invocations (warlock)', 107).
phb_index('eldritch invocations', 110-111).
phb_index('eldritch knight (fighter martial archetype)', 74).
phb_index('eldritch master (warlock)', 108).
phb_index('eldritch sight (warlock eldritch invocation)', 110).
phb_index('eldritch spear (warlock eldritch invocation)', 111).
phb_index('eldritch strike (fighter)', 75).
phb_index('electrum piece (ep) (coinage)', 143).
phb_index('elemental affinity (sorcerer)', 103).
phb_index('elemental attunement (monk elemental discipline)', 81).
phb_index('elemental chaos (plane of existence)', 301).
phb_index('elemental disciplines (monk)', 81).
phb_index('elemental planes (plane of existence)', 301).
phb_index('elemental wild shape (druid)', 69).
phb_index('elf', 21-24).
phb_index('elf names', 22-23).
phb_index('elf traits', 23-24).
phb_index('elf weapon training', 23).
phb_index('elusive (rogue)', 96).
phb_index('elvish', 23).
phb_index('alphabet', 123).
phb_index('language', 23).
phb_index('elysium (plane of existence)', 302).
phb_index('empowered evocation (wizard)', 117).
phb_index('empowered spell (sorcerer metamagic)', 102).
phb_index('empty body (monk)', 79).
phb_index('enchantment', 117).
phb_index('school (wizard arcane tradition)', 117).
phb_index('enchantment savant (wizard)', 117).
phb_index('enchanter', 117).
phb_index('encounters (travel)', 183).
phb_index('encumbrance (lifting and carrying)', 176).
phb_index('entertainer (background)', 130-131).
phb_index('entropic ward (warlock)', 110).
phb_index('environment', 14).
phb_index('adventuring gear', 148).
phb_index('armor and shields', 144-146).
phb_index('background', 125).
phb_index('mounts and vehicles', 155).
phb_index('packs', 151).
phb_index('size (variant)', 144).
phb_index('starting', 143).
phb_index('tools', 154).
phb_index('weapon', 14).
phb_index('see also specific background entries under background', 125).
phb_index('equipment', 143).
phb_index('equipment packs', 151).
phb_index('see also specific class entries: quick build', 45).
phb_index('escape the horde (ranger defensive tactics)', 93).
phb_index('escaping a grapple', 195).
phb_index('eternal mountain defense (monk elemental discipline)', 81).
phb_index('ethereal plane (plane of existence)', 301).
phb_index('evasion', null).
phb_index('monk', 79).
phb_index('ranger (superior hunter\'s defense)', 93).
phb_index('rogue', 96).
phb_index('evasive footwork maneuver (fighter maneuver)', 74).
phb_index('evocation', 117-118).
phb_index('school (wizard arcane tradition)', 117-118).
phb_index('evocation savant (wizard)', 117).
phb_index('evoker', 117).
phb_index('exceptional training (ranger)', 93).
phb_index('exception-based rules', 7).
phb_index('exhaustion', 181).
phb_index('exotic language (language)', 123).
phb_index('expenses', 157-158).
phb_index('experience points (xp)', 15).
phb_index('multiclassing and', 163).
phb_index('expert divination (wizard)', 116).
phb_index('expertise', null).
phb_index('bard', 54).
phb_index('rogue', 96).
phb_index('exploration', 8).
phb_index('extended spell (sorcerer metamagic)', 102).
phb_index('extra attack', null).
phb_index('barbarian', 49).
phb_index('bard', 55).
phb_index('fighter', 72).
phb_index('monk', 79).
phb_index('paladin', 85).
phb_index('ranger', 92).
phb_index('eyes of the rune keeper (warlock eldritch invocation)', 111).
phb_index('faerie (feywild, the; plane of existence)', 300).
phb_index('falling', 183).
phb_index('false identity (charlatan)', 128).
phb_index('familiar (warlock)', 107).
phb_index('fangs of the fire snake (monk elemental discipline)', 81).
phb_index('far realm (plane of existence)', 302).
phb_index('fast hands (rogue)', 97).
phb_index('fast movement (barbarian)', 49).
phb_index('favored enemy (ranger)', 91).
phb_index('feats', 165-170).
phb_index('gaining', 165).
phb_index('prerequisites', 165).
phb_index('feinting attack maneuver (fighter maneuver)', 74).
phb_index('feral instinct (barbarian)', 49).
phb_index('feral senses (ranger)', 92).
phb_index('fey ancestry', null).
phb_index('elf', 23).
phb_index('half-elf', 39).
phb_index('fey presence (warlock)', 108).
phb_index('feywild, the (plane of existence)', 300).
phb_index('fiendish resilience (warlock)', 10).
phb_index('fiendish vigor (warlock eldritch invocation)', 111).
phb_index('fiend, the (warlock otherworldly patron)', 109).
phb_index('fighter', 45).
phb_index('martial archetypes', 72).
phb_index('quick build', 71).
phb_index('fighting style', null).
phb_index('fighter', 72).
phb_index('paladin', 84).
phb_index('ranger', 91).
phb_index('fighting styles', 72).
phb_index('archery', 72).
phb_index('defense', 72).
phb_index('dueling', 72).
phb_index('great weapon fighting', 72).
phb_index('protection', 72).
phb_index('two-weapon fighting', 72).
phb_index('finding a hidden creature', 177).
phb_index('finding a hidden object', 178).
phb_index('finesse (weapon property)', 147).
phb_index('fire damage (damage type)', 196).
phb_index('fist of four thunders (monk elemental discipline)', 81).
phb_index('fist of unbroken air (monk elemental discipline)', 81).
phb_index('flames of the phoenix (monk elemental discipline)', 81).
phb_index('flaws (personality)', 124).
phb_index('fleet of foot (wood elf)', 24).
phb_index('flexible casting (sorcerer font of magic)', 101).
phb_index('flurry of blows (monk ki)', 78).
phb_index('flying (movement)', 191).
phb_index('focused conjuration (wizard)', 116).
phb_index('foe slayer (ranger)', 92).
phb_index('folk hero (background)', 131-132).
phb_index('font of inspiration (bard)', 54).
phb_index('font of magic (sorcerer)', 101).
phb_index('food and drink', 158).
phb_index('expenses', 158).
phb_index('food requirements', 185).
phb_index('water requirements', 185).
phb_index('foraging (travel)', 183).
phb_index('force damage (damage type)', 196).
phb_index('forced march (movement)', 181).
phb_index('forest gnome (race; gnome)', 37).
phb_index('frenzy (barbarian)', 49).
phb_index('frightened (condition)', 290).
phb_index('frog (creature)', 305).
phb_index('gaining a level', 15).
phb_index('gargantuan (size category)', 191).
phb_index('gaze of two minds (warlock eldritch invocation)', 111).
phb_index('gehenna (plane of existence)', 302).
phb_index('gender (character: sex and gender)', 121).
phb_index('getting into and out of armor (armor and shields)', 146).
phb_index('giant (language)', 123).
phb_index('giant killer (ranger hunter\'s prey)', 93).
phb_index('gladiator (background)', 131).
phb_index('gnome', 35-37).
phb_index('gnome cunning (gnome)', 37).
phb_index('gnome names', 36).
phb_index('gnome traits', 36-37).
phb_index('gnomish (language)', 37).
phb_index('goading attack maneuver (fighter maneuver)', 74).
phb_index('gold dwarf (race; dwarf: hill dwarf)', 20).
phb_index('gold piece (gp)', 14).
phb_index('coinage', 143).
phb_index('gong of the summit (monk elemental discipline)', 81).
phb_index('grappled (condition)', 290).
phb_index('grappling', 195).
phb_index('gray dwarf (duergar) (race; dwarf)', 20).
phb_index('greater portent (wizard)', 117).
phb_index('great old one, the (warlock otherworldly patron)', 109-110).
phb_index('great weapon fighting (fighting style)', 72).
phb_index('great wheel, the (plane of existence)', 302).
phb_index('grid (variant)', 192).
phb_index('grim harvest (wizard)', 118).
phb_index('group checks (ability check)', 175).
phb_index('guided strike (channel divinity cleric option)', 63).
phb_index('guild artisan (background)', 132-133).
phb_index('guild membership (guild artisan)', 133).
phb_index('guild merchant (background)', 133).
phb_index('hades (plane of existence)', 302).
phb_index('half cover (cover)', 196).
phb_index('half-elf', 38-39).
phb_index('half-elf names', 39).
phb_index('half-elf traits', 39).
phb_index('halfling', 26-28).
phb_index('hallfing (language)', 28).
phb_index('halfling names', 27).
phb_index('halfling nimbleness', 28).
phb_index('halfling traits', 28).
phb_index('half-orc', 40-41).
phb_index('half-orc names', 41).
phb_index('half-orc traits', 41).
phb_index('hawk (falcon) (creature)', 306).
phb_index('healing', 197).
phb_index('hearing (wisdom: perception)', 178).
phb_index('condition: deafened', 290).
phb_index('heavily obscured', 183).
phb_index('heavy (weapon property)', 147).
phb_index('heavy armor (armor and shields)', 145).
phb_index('movement: in heavy armor', 144).
phb_index('heavy weapons and small creatures', 147).
phb_index('height and weight (character)', 121).
phb_index('heightened spell (sorcerer metamagic)', 102).
phb_index('hellish resistance (tiefling)', 43).
phb_index('help action', 192).
phb_index('hermit (background)', 134-135).
phb_index('hidden', 177).
phb_index('dexterity: stealth', 177).
phb_index('finding a hidden creature', 177).
phb_index('finding a hidden object', 178).
phb_index('hiding', 177).
phb_index('unseen attackers and targets', 194-195).
phb_index('hide action', 192).
phb_index('hide in plain sight (ranger)', 92).
phb_index('hiding', 177).
phb_index('high elf (race; elf)', 23).
phb_index('hill dwarf (race; dwarf)', 20).
phb_index('hirelings', 159).
phb_index('history (intelligence skill)', 177-178).
phb_index('hit dice', 12).
phb_index('multiclassing and', 163).
phb_index('see also specific class entries', 45).
phb_index('hit point maximum', 12).
phb_index('hit points', 12).
phb_index('constitution and', 177).
phb_index('current', 196).
phb_index('damage at 0 (death saving throw)', 197).
phb_index('dropping to 0', 197-198).
phb_index('increasing with level', 15).
phb_index('multiclassing and', 163).
phb_index('starting', 12).
phb_index('subtracting damage from', 196).
phb_index('temporary', 198).
phb_index('holding breath', 183).
phb_index('holy nimbus (paladin)', 86).
phb_index('holy symbol', null).
phb_index('cleric', 57).
phb_index('paladin', 84).
phb_index('horde breaker (ranger hunter\'s prey)', 93).
phb_index('horse, riding (creature)', 310).
phb_index('see also mounts and vehicles', 155).
phb_index('how to play (rules)', 6).
phb_index('huge (size category)', 191).
phb_index('human', 29-31).
phb_index('human ethnicities', 30-31).
phb_index('human names', 30).
phb_index('human traits', 31).
phb_index('variant', 31).
phb_index('hunter (ranger archetype)', 93).
phb_index('hunter\'s prey (ranger)', 93).
phb_index('hurl through hell (warlock)', 109).
phb_index('hustle (dash action)', 192).
phb_index('hypnotic gaze (wizard)', 117).
phb_index('ideals (personality)', 124).
phb_index('ignan (language)', 123).
phb_index('illusion', 118).
phb_index('school (wizard arcane tradition)', 118).
phb_index('illusionist', 118).
phb_index('illusion savant (wizard)', 118).
phb_index('illuskan (human ethnicity)', 31).
phb_index('illusory reality (wizard)', 118).
phb_index('illusory self (wizard)', 118).
phb_index('imp (creature)', 306).
phb_index('impostor (rogue)', 97).
phb_index('improved abjuration (wizard)', 115).
phb_index('improved combat superiority (fighter)', 74).
phb_index('improved critical (fighter)', 72).
phb_index('improved divine smite (paladin)', 85).
phb_index('improved duplicity (cleric)', 63).
phb_index('improved flare (cleric)', 61).
phb_index('improved minor illusion (wizard)', 118).
phb_index('improved war magic (fighter)', 75).
phb_index('improvised weapons', 147-148).
phb_index('incapacitated (condition)', 290).
phb_index('indomitable (fighter)', 72).
phb_index('indomitable might (barbarian)', 49).
phb_index('infernal (language)', 123).
phb_index('infernal legacy (tiefling)', 43).
phb_index('infiltration expertise (rogue)', 97).
phb_index('initiative', 177).
phb_index('inner planes (plane of existence)', 301).
phb_index('insight (wisdom skill)', 178).
phb_index('inspiration', 125).
phb_index('instantaneous (spell duration)', 203).
phb_index('instinctive charm (wizard)', 117).
phb_index('intelligence', 12).
phb_index('arcana', 177).
phb_index('checks', 177-178).
phb_index('history', 177-178).
phb_index('investigation', 178).
phb_index('nature', 178).
phb_index('religion', 178).
phb_index('intimidating presence (barbarian)', 49-50).
phb_index('intimidation (charisma skill)', 179).
phb_index('inured to undeath (wizard)', 119).
phb_index('investigation (intelligence skill)', 178).
phb_index('invisible (condition)', 291).
phb_index('unseen attackers and targets', 194-195).
phb_index('invoke duplicity (channel divinity cleric option)', 63).
phb_index('jack of all trades (bard)', 54).
phb_index('jumping (movement)', 182).
phb_index('keen senses (elf)', 23).
phb_index('ki (monk)', 78).
phb_index('saving throws', 78).
phb_index('ki-empowered strikes (monk)', 79).
phb_index('ki points', 78).
phb_index('spells and', 80).
phb_index('knight (background: noble knight)', 136).
phb_index('knocking a creature out', 198).
phb_index('know your enemy (fighter)', 73-74).
phb_index('knowledge domain (cleric divine domain)', 59-60).
phb_index('knowledge of the ages (channel divinity cleric option)', 59).
phb_index('land\'s stride', null).
phb_index('druid', 69).
phb_index('ranger', 92).
phb_index('language', 17).
phb_index('druidic', 66).
phb_index('thieves\' cant', 96).
phb_index('large (size category)', 191).
phb_index('lawful evil (alignment)', 122).
phb_index('lawful good (alignment)', 122).
phb_index('lawful neutral (alignment)', 122).
phb_index('lay on hands (paladin)', 84).
phb_index('learning spells. see specific class spellcasting entries: bard (52); cleric (58); druid (66); fighter, eldritch knight (75); ranger (91); rogue, arcane trickster (98); sorcerer (101); warlock (107); wizard (114)', null).
phb_index('lethal damage', 197).
phb_index('level', 11).
phb_index('life domain (cleric divine domain)', 60).
phb_index('lifedrinker (warlock eldritch invocation)', 111).
phb_index('lifestyle (expenses)', 157).
phb_index('lifting and carrying', 176).
phb_index('carrying capacity', 176).
phb_index('encumbrance (variant)', 176).
phb_index('size and', 176).
phb_index('light', 183).
phb_index('light (weapon property)', 147).
phb_index('light armor (armor and shields)', 144).
phb_index('light domain (cleric divine domain)', 60-61).
phb_index('lightly obscured', 183).
phb_index('lightning damage (damage type)', 196).
phb_index('limbo (plane of existence)', 302).
phb_index('line (area of effect)', 205).
phb_index('line of sight (casting a spell: targeting)', 204).
phb_index('lion (creature)', 307).
phb_index('listening (wisdom: perception)', 178).
phb_index('deafened (condition)', 290).
phb_index('loading (weapon property)', 147).
phb_index('lock, opening or picking (thieves\' tools)', 154).
phb_index('lodging (expenses)', 158).
phb_index('lolth', 24).
phb_index('long rest (resting)', 186).
phb_index('lower planes (plane of existence)', 302).
phb_index('low-light vision (darkvision)', 183).
phb_index('lucky (halfling)', 28).
phb_index('lunging attack maneuver (fighter maneuver)', 74).
phb_index('mage hand legerdemain (rogue)', 98).
phb_index('magical ambush (rogue)', 98).
phb_index('magical secrets (bard)', 54).
phb_index('magic', 8).
phb_index('see also specific class entries', 45).
phb_index('magic item (wealth)', 144).
phb_index('magic-user (wizard)', 45).
phb_index('arcane magic', 205).
phb_index('malleable illusions (wizard)', 118).
phb_index('maneuver (fighter)', 73).
phb_index('maneuvering attack maneuver (fighter maneuver)', 74).
phb_index('maneuvers', 73).
phb_index('list', 74).
phb_index('saving throws', 73).
phb_index('mapping (travel)', 183).
phb_index('marching order (travel)', 182).
phb_index('martial archetype (fighter)', 72).
phb_index('martial archetypes', 72-75).
phb_index('battle master', 73-74).
phb_index('champion', 72-73).
phb_index('eldritch knight', 74-75).
phb_index('martial arts (monk)', 78).
phb_index('martial weapon (weapon category)', 146).
phb_index('mask of many faces (warlock eldritch invocation)', 111).
phb_index('mask of the wild (wood elf)', 24).
phb_index('master of myriad forms (warlock eldritch invocation)', 111).
phb_index('master of nature (cleric)', 62).
phb_index('master transmuter (wizard)', 119).
phb_index('mastiff (creature)', 307).
phb_index('mounts and vehicles', 155).
phb_index('material spell component', 203).
phb_index('material plane (plane of existence)', 300).
phb_index('mechanus (plane of existence)', 302).
phb_index('medicine (wisdom skill)', 178).
phb_index('medium (size category)', 191).
phb_index('medium armor (armor and shields)', 144).
phb_index('melee attack', 195).
phb_index('unarmed', 195).
phb_index('with a weapon that requires ammunition', 147).
phb_index('reach', 195).
phb_index('melee weapon', 14).
phb_index('menacing (half-orc)', 41).
phb_index('menacing attack maneuver (fighter maneuver)', 74).
phb_index('metamagic (sorcerer)', 101-102).
phb_index('military rank (soldier)', 140).
phb_index('mindless rage (barbarian)', 49).
phb_index('minions of chaos (warlock eldritch invocation)', 111).
phb_index('minor alchemy (wizard)', 119).
phb_index('minor conjuration (wizard)', 116).
phb_index('mire the mind (warlock eldritch invocation)', 111).
phb_index('mist stance (monk elemental discipline)', 81).
phb_index('misty escape (warlock)', 109).
phb_index('misty visions (warlock eldritch invocation)', 111).
phb_index('modifier', 7).
phb_index('ability', 7).
phb_index('bonus', 7).
phb_index('penalty', 7).
phb_index('monastic orders', 81).
phb_index('monastic tradition (monk)', 78).
phb_index('monastic traditions', 79-81).
phb_index('way of the open hand', 79-80).
phb_index('way of shadow', 80).
phb_index('way of the four elements', 80-81).
phb_index('monk', 45).
phb_index('monastic traditions', 79-81).
phb_index('quick build', 77).
phb_index('monk weapons', 78).
phb_index('moon elf (race; elf: high elf)', 23).
phb_index('mount (mounts and vehicles)', 155).
phb_index('mountain dwarf (race; dwarf)', 20).
phb_index('mount celestia (plane of existence)', 302).
phb_index('mounted combat', 198).
phb_index('mounts and vehicles', 155).
phb_index('movement', 181-182).
phb_index('move action', 189).
phb_index('movement', 181-183).
phb_index('around creatures', 191).
phb_index('breaking up', 190).
phb_index('climbing', 182).
phb_index('crawling', 182).
phb_index('combat', 190-192).
phb_index('difficult terrain', 182).
phb_index('flying', 191).
phb_index('prone', 190-191).
phb_index('forced march', 181).
phb_index('in heavy armor', 144).
phb_index('jumping', 182).
phb_index('mounts and vehicles', 181-182).
phb_index('size', 191-192).
phb_index('speed', 14).
phb_index('squeezing', 192).
phb_index('stand up', 190-191).
phb_index('swimming', 182).
phb_index('travel pace', 181).
phb_index('using different speeds', 190).
phb_index('moving a grappled creature', 195).
phb_index('mulan (human ethnicity)', 31).
phb_index('mule (creature)', 307).
phb_index('multiattack (ranger)', 93).
phb_index('multiattack defense (ranger defensive tactic)', 93).
phb_index('multiclassing', 163-164).
phb_index('channel divinity', 164).
phb_index('experience points', 163).
phb_index('hit points and hit dice', 163).
phb_index('pact magic', 164).
phb_index('prerequisites', 163).
phb_index('proficiencies', 163-164).
phb_index('proficiency bonus', 163).
phb_index('spell slots', 164).
phb_index('spellcasting', 164).
phb_index('spells known and prepared', 164).
phb_index('unarmored defense', 164).
phb_index('multiverse', 5-6).
phb_index('mystic arcanum (warlock)', 108).
phb_index('name (character)', 121).
phb_index('natural explorer (ranger)', 91).
phb_index('natural illusionist (forest gnome)', 37).
phb_index('naturally stealthy (lightfoot halfling)', 28).
phb_index('natural recovery (druid)', 68).
phb_index('nature (intelligence skill)', 178).
phb_index('nature domain (cleric divine domain)', 61-62).
phb_index('nature\'s sanctuary (druid)', 69).
phb_index('nature\'s ward (druid)', 69).
phb_index('nature\'s wrath (channel divinity paladin option)', 87).
phb_index('navigating (travel)', 183).
phb_index('necromancer', 118-119).
phb_index('necromancy', 118).
phb_index('school (wizard arcane tradition)', 118-119).
phb_index('necromancy savant (wizard)', 118).
phb_index('necrotic damage (damage type)', 196).
phb_index('negative plane (plane of existence)', 300).
phb_index('neutral (alignment)', 122).
phb_index('neutral evil (alignment)', 122).
phb_index('neutral good (alignment)', 122).
phb_index('nine hells, the (plane of existence)', 302).
phb_index('noble (background)', 135-136).
phb_index('noble knight (background)', 136).
phb_index('nonplayer character (npc)', 8).
phb_index('nonlethal damage', 196).
phb_index('npc (nonplayer character)', 8).
phb_index('oath of devotion (paladin sacred oath)', 85-86).
phb_index('oath of the ancients (paladin sacred oath)', 86-87).
phb_index('oath of vengeance (paladin sacred oath)', 87-88).
phb_index('oath spells (paladin)', 85-88).
phb_index('objects', 185).
phb_index('attacking', 185).
phb_index('interacting with', 185).
phb_index('using during combat', 190).
phb_index('one with shadows (warlock eldritch invocation)', 111).
phb_index('open hand technique (monk)', 79).
phb_index('open locks (thieves\' tools)', 154).
phb_index('opportunist (monk)', 80).
phb_index('opportunity attack', 195).
phb_index('orc', 41).
phb_index('language', 41).
phb_index('orison (cantrip)', 201).
phb_index('otherworldly leap (warlock eldritch invocation)', 111).
phb_index('otherworldly patron (warlock)', 107).
phb_index('otherworldly patrons', 108-110).
phb_index('archfey, the', 108-109).
phb_index('fiend, the', 109).
phb_index('great old one, the', 109-110).
phb_index('outer planes (plane of existence)', 300).
phb_index('outlander (background)', 136-137).
phb_index('outlands, the (plane of existence)', 302).
phb_index('overchannel (wizard)', 118).
phb_index('owl (creature)', 308).
phb_index('pact boon (warlock)', 107-108).
phb_index('pact of the blade', 107-108).
phb_index('pact of the chain', 107).
phb_index('pact of the tome', 108).
phb_index('your pact boon', 108).
phb_index('pact magic (warlock)', 107).
phb_index('multiclassing and', 164).
phb_index('pact weapon (warlock)', 107-108).
phb_index('paladin', 45).
phb_index('quick build', 83).
phb_index('sacred oaths', 85-88).
phb_index('spell list', 208-209).
phb_index('pandemonium (plane of existence)', 302).
phb_index('panther (creature)', 308).
phb_index('paralyzed (condition)', 291).
phb_index('parry maneuver (fighter maneuver)', 74).
phb_index('party (adventuring)', 15).
phb_index('passive check (ability check)', 175).
phb_index('path of the berserker (barbarian primal path)', 49-50).
phb_index('path of the totem warrior (barbarian primal path)', 50).
phb_index('patient defense (monk ki)', 78).
phb_index('peerless skill (bard)', 55).
phb_index('penalty', 6).
phb_index('percentile (dice)', 6).
phb_index('perception (wisdom skill)', 178).
phb_index('travel: noticing threats', 182-183).
phb_index('perfect self (monk)', 79).
phb_index('performance (charisma skill)', 179).
phb_index('persistent rage (barbarian)', 49).
phb_index('personality (character)', 122-124).
phb_index('persuasion (charisma skill)', 179).
phb_index('petrified (condition)', 291).
phb_index('pick locks (thieves\' tools)', 154).
phb_index('pick pockets (dexterity: sleight of hand)', 177).
phb_index('piercing damage (damage type)', 196).
phb_index('pirate (background)', 139).
phb_index('planar travel', 301).
phb_index('plane of air (plane of existence)', 301).
phb_index('plane of earth (plane of existence)', 301).
phb_index('plane of faerie (plane of existence)', 301).
phb_index('plane of fire (plane of existence)', 301).
phb_index('plane of shadow (plane of existence)', 301).
phb_index('plane of water (plane of existence)', 301).
phb_index('planes of existence', 5).
phb_index('platinum piece (pp) (coinage)', 143).
phb_index('player character', 5).
phb_index('point of origin (area of effect)', 204).
phb_index('poison damage (damage type)', 196).
phb_index('poisoned (condition)', 292).
phb_index('position of privilege (noble)', 135).
phb_index('positive plane (plane of existence)', 300).
phb_index('portent (wizard)', 116).
phb_index('potent cantrip (wizard)', 117).
phb_index('potent spellcasting (cleric)', null).
phb_index('knowledge domain', 60).
phb_index('light domain', 61).
phb_index('precision attack maneuver (fighter maneuver)', 74).
phb_index('preparing spells. see spellcasting: (cleric)', 58).
phb_index('preserve life (channel divinity cleric options)', 60).
phb_index('primal champion (barbarian)', 49).
phb_index('primal path (barbarian)', 48).
phb_index('primal paths', 49-50).
phb_index('path of the berserker', 49-50).
phb_index('path of the totem warrior', 50).
phb_index('primal strike (druid)', 69).
phb_index('primeval awareness (ranger)', 92).
phb_index('primordial (language)', 123).
phb_index('profession (downtime activity)', 187).
phb_index('proficiencies', 11).
phb_index('multiclassing and', 163-164).
phb_index('saving throw', 14).
phb_index('skill', 14).
phb_index('spell', 14).
phb_index('tool', 14).
phb_index('vehicle', 155).
phb_index('weapon', 14).
phb_index('proficiency bonus', 14).
phb_index('adding', 14).
phb_index('dividing', 14).
phb_index('increasing with level', 15).
phb_index('multiclassing and', 163).
phb_index('multiplying', 14).
phb_index('projected ward (wizard)', 115).
phb_index('projectile weapon (weapon property: ammunition)', 146).
phb_index('prone', 190-191).
phb_index('prone condition', 292).
phb_index('protection (fighting style)', 72).
phb_index('pseudodragon (creature)', 308).
phb_index('psychic damage (damage type)', 196).
phb_index('punching (unarmed melee attack)', 195).
phb_index('purity of body (monk)', 79).
phb_index('purity of spirit (paladin)', 86).
phb_index('pushing attack maneuver (fighter maneuver)', 74).
phb_index('quasit (creature)', 309).
phb_index('quick build', 11).
phb_index('see also under specific class entries', 45).
phb_index('quickened spell (sorcerer metamagic)', 102).
phb_index('quivering palm (monk)', 80).
phb_index('race', 11).
phb_index('choosing a', 11).
phb_index('traits (racial)', 11).
phb_index('see also specific race entries', 17).
phb_index('racial ability score increase (racial traits: ability score increase)', 11).
phb_index('racial traits', 11).
phb_index('ability score increase', 11).
phb_index('age', 17).
phb_index('alignment', 17).
phb_index('languages', 17).
phb_index('size', 17).
phb_index('speed', 17).
phb_index('subrace', 17).
phb_index('see also specific race entries', 17).
phb_index('radiance of the dawn (channel divinity cleric options)', 61).
phb_index('radiant damage (damage type)', 196).
phb_index('rage (barbarian)', 48).
phb_index('rally maneuver (fighter maneuver)', 74).
phb_index('range', 195).
phb_index('maximum', 147).
phb_index('normal', 147).
phb_index('of spells (casting a spell: range)', 202-203).
phb_index('range (weapon property)', 146).
phb_index('ranged attack', 195).
phb_index('in melee', 195).
phb_index('ranged weapon', 14).
phb_index('ranger', 45).
phb_index('quick build', 90).
phb_index('ranger archetypes', 93).
phb_index('spell list', 209).
phb_index('ranger archetype (ranger)', 92).
phb_index('ranger archetypes', 93).
phb_index('beast master', 93).
phb_index('hunter', 93).
phb_index('ranger\'s companion (ranger)', 93).
phb_index('rashemi (human ethnicity)', 31).
phb_index('rat (creature)', 309).
phb_index('raven (creature)', 309).
phb_index('reach (creature)', 195).
phb_index('reach (weapon property)', 147).
phb_index('reaction', 190).
phb_index('casting a spell: casting time', 202).
phb_index('read thoughts (channel divinity cleric options)', 59).
phb_index('ready action', 193).
phb_index('reckless attack (barbarian)', 48).
phb_index('recovering ammunition (weapon property: ammunition)', 146).
phb_index('recuperating (downtime activity)', 187).
phb_index('refitting plate armor (armor and shields; variant: equipment sizes)', 144).
phb_index('relentless (fighter)', 78).
phb_index('relentless avenger (paladin)', 88).
phb_index('relentless endurance (half-orc)', 41).
phb_index('relentless rage (barbarian)', 49).
phb_index('reliable talent (rogue)', 96).
phb_index('religion', 293).
phb_index('cleric', 56).
phb_index('druid', 64).
phb_index('paladin', 82).
phb_index('religion (intelligence skill)', 178).
phb_index('reloading (weapon property: ammunition)', 146).
phb_index('objects: using during combat', 190).
phb_index('weapon properties: loading', 147).
phb_index('remarkable athlete (fighter)', 72).
phb_index('repelling blast (warlock eldritch invocation)', 111).
phb_index('research (downtime activity)', 187).
phb_index('researcher (sage)', 138).
phb_index('resistance (damage resistance)', 97).
phb_index('damage resistance (dragonborn)', 34).
phb_index('resting', 186).
phb_index('restrained (condition)', 292).
phb_index('result (total)', 7).
phb_index('retainers (noble variant)', 136).
phb_index('retaliation (barbarian)', 50).
phb_index('ride the wind (monk elemental discipline)', 81).
phb_index('riposte maneuver (fighter maneuver)', 74).
phb_index('ritual casting. see spellcasting: bard (52-53); cleric (58); druid (66); wizard (114)', null).
phb_index('ritual caster feat', 169).
phb_index('rituals', 201-202).
phb_index('casting a spell: casting time', 202).
phb_index('river of hungry flame (monk elemental discipline)', 81).
phb_index('rock gnome (race; gnome)', 37).
phb_index('rogue', 45).
phb_index('quick build', 95).
phb_index('roguish archetype (rogue)', 96).
phb_index('roguish archetypes', 97-98).
phb_index('arcane trickster', 97-98).
phb_index('assassin', 97).
phb_index('thief', 97).
phb_index('roleplaying', 8).
phb_index('roll', 6-7).
phb_index('round (time)', 181).
phb_index('rounding numbers', 7).
phb_index('rowed vessel', 155-157).
phb_index('rules', 6-7).
phb_index('exceptions', 7).
phb_index('how to play', 6).
phb_index('specific beats general', 7).
phb_index('run (dash action)', 192).
phb_index('rush of the gale spirits (monk elemental discipline)', 81).
phb_index('rustic hospitality (folk hero)', 131).
phb_index('sacred oath (paladin)', 85).
phb_index('oath spells', 85).
phb_index('sacred oaths', 85-88).
phb_index('breaking', 86).
phb_index('oath of devotion', 85-86).
phb_index('oath of the ancients', 86-87).
phb_index('oath of vengeance', 87-88).
phb_index('sacred plants and wood (druid)', 66).
phb_index('sacred weapon (channel divinity paladin option)', 86).
phb_index('sage (background)', 137-138).
phb_index('sailor (background)', 139).
phb_index('savage attacks (half-orc)', 41).
phb_index('saving throw (save)', 7).
phb_index('death', 197).
phb_index('ki', 78).
phb_index('maneuver (saving throws)', 73).
phb_index('proficiency', 14).
phb_index('spell', 205).
phb_index('saving throw modifier', 12).
phb_index('school of abjuration (wizard)', 115-116).
phb_index('school of conjuration (wizard)', 116).
phb_index('school of divination (wizard)', 116-117).
phb_index('school of enchantment (wizard)', 117).
phb_index('school of evocation (wizard)', 117-118).
phb_index('school of illusion (wizard)', 118).
phb_index('school of necromancy (wizard)', 118-119).
phb_index('school of transmutation (wizard)', 119).
phb_index('schools of magic (arcane traditions)', 115-119).
phb_index('sculptor of flesh (warlock eldritch invocation)', 111).
phb_index('sculpt spells (wizard)', 117).
phb_index('second-story work (rogue)', 97).
phb_index('second wind (fighter)', 72).
phb_index('search action', 193).
phb_index('intelligence: investigation', 178).
phb_index('travel (tracking)', 183).
phb_index('wisdom: perception', 178).
phb_index('self', null).
phb_index('casting a spell: range', 202-203).
phb_index('targeting', 204).
phb_index('selling treasure (wealth)', 144).
phb_index('services', 159).
phb_index('sex (character: sex and gender)', 121).
phb_index('shadow arts (monk)', 80).
phb_index('shadowfell, the (plane of existence)', 300).
phb_index('shadow step (monk)', 80).
phb_index('shapechanger (wizard)', 119).
phb_index('shape the flowing river (monk elemental discipline)', 81).
phb_index('share spells (ranger)', 93).
phb_index('shark, reef (creature)', 309).
phb_index('shelter of the faithful (acolyte)', 127).
phb_index('shield (armor and shields)', 144).
phb_index('shield dwarf (race; dwarf: mountain)', 20).
phb_index('ship\'s passage (sailor)', 139).
phb_index('short rest (resting)', 186).
phb_index('shou (human ethnicity)', 31).
phb_index('shoving', 195).
phb_index('sigil (plane of existence)', 302).
phb_index('signature spells (wizard)', 115).
phb_index('sign of ill omen (warlock eldritch invocation)', 111).
phb_index('silvered weapon (weapon)', 148).
phb_index('silver piece (sp) (coinage)', 143).
phb_index('simple weapon (weapon category)', 146).
phb_index('size', 176).
phb_index('and weapons (heavy)', 147).
phb_index('of equipment (variant: equipment sizes)', 144).
phb_index('lifting and carrying: size and', 176).
phb_index('space', 191-192).
phb_index('see also racial traits and specific race trait entries', 17).
phb_index('size category', 191).
phb_index('skeleton (creature)', 310).
phb_index('skill check (ability check: skill)', 174-175).
phb_index('skill versatility (half-elf)', 39).
phb_index('skills with different abilities (variant)', 175).
phb_index('slashing damage (damage type)', 196).
phb_index('sleight of hand (dexterity skill)', 177).
phb_index('slippery mind (rogue)', 96).
phb_index('slow fall (monk)', 78).
phb_index('small (size category)', 191).
phb_index('snake, constrictor (creature)', 305).
phb_index('snake, poisonous (creature)', 308).
phb_index('sneak attack (rogue)', 96).
phb_index('social interaction', 8).
phb_index('soldier (background)', 140-141).
phb_index('somatic spell component', 203).
phb_index('song of rest (bard)', 54).
phb_index('sorcerer', 45).
phb_index('quick build', 100).
phb_index('sorcerous origin', 101).
phb_index('spell list', 209).
phb_index('sorcerous origin', 101).
phb_index('sorcerous origins', 102-104).
phb_index('draconic bloodline', 102-103).
phb_index('wild magic', 103-104).
phb_index('sorcerous restoration', 102).
phb_index('sorcery pointer (sorcerer font of magic)', 101).
phb_index('soul of vengeance (paladin)', 88).
phb_index('space (size)', 191-192).
phb_index('speak with small beasts (forest gnome)', 37).
phb_index('special (weapon property)', 147).
phb_index('speed (movement)', 14).
phb_index('mounts and vehicles', 157).
phb_index('racial traits', 17).
phb_index('travel (pace)', 181).
phb_index('spell', 201).
phb_index('spell attack modifier. see spellcasting: bard (53); cleric (58); druid (66); paladin (85); sorcerer (101); warlock (107); wizard (114)', null).
phb_index('spell attack roll', 205).
phb_index('see also spellcasting: bard (52); cleric (58); druid (66); fighter, eldritch knight (75); ranger (92); rogue, arcane trickster (98); sorcerer (101); warlock (107); wizard (114)', null).
phb_index('spell bombardment (sorcerer)', 103).
phb_index('spellbook (wizard)', 114).
phb_index('spellcaster', 201).
phb_index('spellcasting', 201-205).
phb_index('spellcasting', null).
phb_index('bard', 52-53).
phb_index('cleric', 58).
phb_index('druid', 66).
phb_index('fighter: eldritch knight', 75).
phb_index('multiclassing', 164).
phb_index('paladin', 84-85).
phb_index('ranger', 91-92).
phb_index('rogue: arcane trickster', 97-98).
phb_index('sorcerer', 101).
phb_index('warlock: pact magic', 107).
phb_index('wizard', 114).
phb_index('spellcasting ability', 178).
phb_index('see also spellcasting: bard (52); cleric (58); druid (66); fighter, eldritch knight (75); ranger (91); rogue, arcane trickster (98); sorcerer (101); warlock (107); wizard (114)', null).
phb_index('spellcasting focus. see spellcasting: bard (53); cleric (58); druid (66); paladin (85); sorcerer (101); warlock (107); wizard (114)', null).
phb_index('spell components (casting a spell: components)', 203).
phb_index('material spell component', 203).
phb_index('somatic spell component', 203).
phb_index('verbal spell component', 203).
phb_index('spell descriptions', 211-289).
phb_index('spell level', 201).
phb_index('spell list', 207-211).
phb_index('spell mastery (wizard)', 115).
phb_index('spell preparation. see spellcasting: bard (53); cleric (58); druid (66); paladin (85); sorcerer (101); warlock (107); wizard (114)', null).
phb_index('multiclassing', 165).
phb_index('spell resistance (wizard)', 116).
phb_index('spell save dc', 205).
phb_index('see also spellcasting: bard (53); cleric (58); druid (66); paladin (85); sorcerer (101); warlock (107); wizard (114)', null).
phb_index('spell saving throws', 205).
phb_index('spells known', 201).
phb_index('multiclassing', 164).
phb_index('see also spellcasting: bard (52); cleric (58); druid (66); fighter, eldritch knight (75); ranger (91); rogue, arcane trickster (98); sorcerer (101); warlock (107); wizard (114)', null).
phb_index('spell slots', 201).
phb_index('multiclassing', 164).
phb_index('see also spellcasting: bard (52); cleric (58); druid (66); fighter, eldritch knight (75); ranger (91); rogue, arcane trickster (98); sorcerer (101); warlock (107); wizard (114)', null).
phb_index('spell thief (rogue)', 98).
phb_index('sphere (area of effect)', 205).
phb_index('spider, giant (creature)', 306).
phb_index('spirit seeker (barbarian)', 50).
phb_index('spirit walker (barbarian)', 50).
phb_index('split enchantment (wizard)', 118).
phb_index('sprite (creature)', 310).
phb_index('spy (background)', 130).
phb_index('squeezing (movement)', 192).
phb_index('stabilizing a creature', 197).
phb_index('stable', 197-198).
phb_index('stacking (casting a spell: combining effects)', 205).
phb_index('advantage', 173).
phb_index('disadvantage', 173).
phb_index('standard action', 192).
phb_index('standard language (language)', 123).
phb_index('stand against the tide (ranger superior hunter\'s defense)', 93).
phb_index('starting wealth', 143).
phb_index('starvation (food and drink: food requirements)', 185).
phb_index('stealth (dexterity skill)', 177).
phb_index('armor and shields: stealth', 144).
phb_index('hiding', 177).
phb_index('travel', 182).
phb_index('steel will (ranger defensive tactic)', 93).
phb_index('step of the wind (monk ki)', 78).
phb_index('stillness of mind (monk)', 79).
phb_index('stonecunning (dwarf)', 20).
phb_index('stormborn (cleric)', 62).
phb_index('stout resilience (stout halfling)', 28).
phb_index('strength', 12).
phb_index('athletics', 175).
phb_index('checks', 175).
phb_index('stroke of luck (rogue)', 96).
phb_index('strongheart halfling (race; halfling: stout)', 28).
phb_index('student of war (fighter)', 73).
phb_index('stunned (condition)', 292).
phb_index('stunning strike (monk)', 79).
phb_index('subrace', 17).
phb_index('subtle spell (sorcerer metamagic)', 102).
phb_index('suffocating', 183).
phb_index('sun elf (race; elf: high elf)', 23).
phb_index('sunlight sensitivity (drow)', 24).
phb_index('superior critical (fighter)', 73).
phb_index('superior darkvision (drow)', 24).
phb_index('superior hunter\'s defense (ranger)', 93).
phb_index('superior inspiration (bard)', 54).
phb_index('superiority dice (fighter)', 73).
phb_index('supreme healing (cleric)', 60).
phb_index('supreme sneak (rogue)', 97).
phb_index('surprise', 189).
phb_index('travel: surprise', 183).
phb_index('survival (wisdom skill)', 178).
phb_index('navigating', 183).
phb_index('travel: foraging', 183).
phb_index('tracking', 183).
phb_index('survivor (fighter)', 73).
phb_index('svirfneblin (deep gnome) (race; gnome)', 36).
phb_index('sweeping attack maneuver (fighter maneuver)', 74).
phb_index('sweeping cinder strike (monk elemental discipline)', 81).
phb_index('sylvan (language)', 123).
phb_index('targeting a spell (casting a spell: targeting)', 204).
phb_index('target number', 7).
phb_index('armor class (ac)', 7).
phb_index('difficulty class (dc)', 7).
phb_index('tempest domain (cleric divine domain)', 62).
phb_index('temporary hit points', 198).
phb_index('tenets of devotion (paladin)', 86).
phb_index('tenets of the ancients (paladin)', 86-87).
phb_index('tenets of vengeance (paladin)', 88).
phb_index('terran (language)', 123).
phb_index('tethyrian (human ethnicity)', 31).
phb_index('thac0 (attack roll)', 7).
phb_index('thief (roguish archetype)', 97).
phb_index('thief of five fates (warlock eldritch invocation)', 111).
phb_index('thief\'s reflexes (rogue)', 97).
phb_index('thieves\' cant (rogue language)', 96).
phb_index('thieves\' tools', 154).
phb_index('third eye, the (wizard)', 116-117).
phb_index('thirsting blade (warlock eldritch invocation)', 111).
phb_index('thought shield (warlock)', 110).
phb_index('thousand forms (druid)', 69).
phb_index('three-quarters cover', 196).
phb_index('thrown (weapon property)', 147).
phb_index('thunderbolt strike (cleric)', 62).
phb_index('thunder damage (damage type)', 196).
phb_index('tides of chaos (sorcerer)', 103).
phb_index('tiefling', 42-43).
phb_index('tiefling names', 43).
phb_index('tiefling traits', 43).
phb_index('tiers of play', 15).
phb_index('tiger (creature)', 311).
phb_index('time', 181).
phb_index('day', 181).
phb_index('minute', 181).
phb_index('round', 181).
phb_index('turn', 189).
phb_index('timeless body', null).
phb_index('druid', 67).
phb_index('monk', 79).
phb_index('tinker (rock gnome)', 37).
phb_index('tiny (size category)', 191).
phb_index('tongue of the sun and moon (monk)', 79).
phb_index('tools', 154).
phb_index('total', 7).
phb_index('total cover', 196).
phb_index('totemic attunement (barbarian)', 50).
phb_index('totem spirit (barbarian)', 50).
phb_index('tracking (travel)', 183).
phb_index('trade goods', 144).
phb_index('training (downtime activity)', 187).
phb_index('traits (personality)', 123).
phb_index('see also specific race trait entries', 17).
phb_index('trance (elf)', 23).
phb_index('transitive planes (plane of existence)', 301).
phb_index('tranquility (monk)', 80).
phb_index('transmutation', 119).
phb_index('school (wizard arcane tradition)', 119).
phb_index('transmutation savant (wizard)', 119).
phb_index('transmuter', 119).
phb_index('transmuter\'s stone (wizard)', 119).
phb_index('traps, finding', null).
phb_index('intelligence: investigation', 178).
phb_index('wisdom: perception', 178).
phb_index('traps, removing or disabling (thieves\' tools)', 154).
phb_index('travel', 181-183).
phb_index('drawing a map', 183).
phb_index('encounters', 183).
phb_index('foraging', 183).
phb_index('marching order', 182).
phb_index('navigating', 183).
phb_index('noticing threats', 182-183).
phb_index('pace', 181).
phb_index('stealth', 182).
phb_index('surprise', 183).
phb_index('tracking', 183).
phb_index('trickery domain (cleric divine domain)', 62-63).
phb_index('trinkets', 160-161).
phb_index('trip attack maneuver (fighter maneuver)', 74).
phb_index('truesight', 185).
phb_index('turami (human ethnicity)', 31).
phb_index('turn (time)', 189).
phb_index('turn, taking yours', 189-190).
phb_index('action', 189).
phb_index('bonus action', 189).
phb_index('movement', 181-183).
phb_index('turn the faithless (channel divinity paladin option)', 87).
phb_index('turn the unholy (channel divinity paladin option)', 86).
phb_index('turn undead (channel divinity cleric option)', 59).
phb_index('twinned spell (sorcerer metamagic)', 102).
phb_index('two-handed (weapon property)', 147).
phb_index('two-weapon fighting', 195).
phb_index('two-weapon fighting (class feature: fighting style)', null).
phb_index('fighter', 72).
phb_index('ranger', 91).
phb_index('unarmed (melee attack)', 195).
phb_index('unarmored defense', null).
phb_index('barbarian', 48).
phb_index('monk', 78).
phb_index('unarmored movement (monk)', 78).
phb_index('uncanny dodge', null).
phb_index('ranger superior hunter\'s defense', 93).
phb_index('rogue class feature', 96).
phb_index('undead thralls (wizard)', 119).
phb_index('underdark', 17).
phb_index('undying sentinel (paladin)', 87).
phb_index('uncommon races', 33).
phb_index('unconscious', 197).
phb_index('condition', 292).
phb_index('undercommon (language)', 123).
phb_index('underwater combat', 198).
phb_index('unseen attackers and targets', 194-195).
phb_index('upper planes (plane of existence)', 302).
phb_index('urchin (background)', 141).
phb_index('use an object action', 193).
phb_index('use magic device (rogue)', 97).
phb_index('using this book', 6).
phb_index('vanish (ranger)', 92).
phb_index('vehicle (mounts and vehicles)', 155).
phb_index('verbal spell component', 203).
phb_index('versatile (weapon property)', 147).
phb_index('versatile trickster (rogue)', 98).
phb_index('vision', 183).
phb_index('blindsight', 183).
phb_index('darkvision', 183).
phb_index('truesight', 185).
phb_index('wisdom: perception', 178).
phb_index('visions of distant realms (warlock eldritch invocation)', 111).
phb_index('visions of the past (cleric)', 60).
phb_index('voice of the chain master (warlock eldritch invocation)', 111).
phb_index('volley (ranger multiattack)', 93).
phb_index('vow of enmity (channel divinity paladin option)', 88).
phb_index('vulnerability (damage vulnerability)', 197).
phb_index('wanderer (outlander)', 136).
phb_index('warding flare (cleric)', 61).
phb_index('war domain (cleric divine domain)', 63).
phb_index('war god\'s blessing (channel divinity cleric option)', 63).
phb_index('warhorse (creature)', 311).
phb_index('mounts and vehicles', 155).
phb_index('war magic (fighter)', 75).
phb_index('war priest (cleric)', 63).
phb_index('water whip (monk elemental discipline)', 81).
phb_index('warlock', 45).
phb_index('quick build', 106).
phb_index('otherworldly patrons', 108-110).
phb_index('spell list', 210).
phb_index('wave of rolling earth (monk elemental discipline)', 81).
phb_index('wealth', 143-144).
phb_index('expenses', 157-158).
phb_index('magic item', 144).
phb_index('selling treasure', 144).
phb_index('starting', 143).
phb_index('weapon', 14).
phb_index('improvised', 147-148).
phb_index('silvered', 148).
phb_index('table', 149).
phb_index('weapon bond (fighter)', 75).
phb_index('weapon categories', 146).
phb_index('weapon proficiency', 14).
phb_index('see also specific classes: proficiencies', 45).
phb_index('see also specific race trait entries', 17).
phb_index('weapon properties', 146-147).
phb_index('weave, the', 205).
phb_index('whirlwind attack (ranger multiattack)', 93).
phb_index('whispers of the grave (warlock eldritch invocation)', 111).
phb_index('wholeness of body (monk)', 79-80).
phb_index('wild elf (race; elf: wood elf)', 24).
phb_index('wild magic (sorcerer sorcerous origin)', 103).
phb_index('wild magic surge (sorcerer)', 103).
phb_index('table', 104).
phb_index('wild shape (druid)', 66-67).
phb_index('wisdom', 12).
phb_index('animal handling', 178).
phb_index('checks', 178).
phb_index('insight', 178).
phb_index('medicine', 178).
phb_index('perception', 178).
phb_index('survival', 178).
phb_index('witch sight (warlock eldritch invocation)', 111).
phb_index('wizard', 45).
phb_index('arcane traditions', 115-119).
phb_index('quick build', 113).
phb_index('spell list', 210-211).
phb_index('wolf, dire (creature)', 305).
phb_index('wolf (creature)', 311).
phb_index('wood elf (race; elf)', 24).
phb_index('world', 5-6).
phb_index('planes of existence: material plane', 302).
phb_index('wraith of the storm (cleric)', 62).
phb_index('xp (experience points)', 15).
phb_index('ysgard (plane of existence)', 302).
phb_index('zombie (creature)', 311).

