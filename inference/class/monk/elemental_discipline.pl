:- discontiguous elemental_discipline_option/1,
                 elemental_discipline_spell/3,
                 elemental_discipline_spell_self_target_only/2.

meta_todo('elemental discipline', "Omit cards for disciplines that just add a spell.").

elemental_discipline_option('breath of winter') :- monk('four elements') >: 17.
elemental_discipline_spell('breath of winter', 'cone of cold', 6).

elemental_discipline_option('clench of the north wind') :- monk('clench of the north wind') >: 6.
elemental_discipline_spell('clench of the north wind', 'hold person', 3).

elemental_discipline_option('elemental attunement').

elemental_discipline_option('eternal mountain defense') :- monk('four elements') >: 17.
elemental_discipline_spell('eternal mountain defense', stoneskin, 5).
elemental_discipline_spell_self_target_only('eternal mountain defense', stoneskin).

elemental_discipline_option('fangs of the fire snake').

elemental_discipline_option('fist of four thunders').
elemental_discipline_spell('fist of four thunders', thunderwave, 2).

elemental_discipline_option('fist of unbroken air').

elemental_discipline_option('flames of the phoenix') :- monk('four elements') >: 11.
elemental_discipline_spell('flames of the phoenix', fireball, 4).

elemental_discipline_option('gong of the summit') :- monk('four elements') >: 6.
elemental_discipline_spell('gong of the summit', shatter, 3).

elemental_discipline_option('mist stance') :- monk('four elements') >: 11.
elemental_discipline_spell('mist stance', 'gaseous form', 4).
elemental_discipline_spell_self_target_only('mist stance', 'gaseous form').

elemental_discipline_option('ride the wind') :- monk('four elements') >: 11.
elemental_discipline_spell('ride the wind', fly, 4).
elemental_discipline_spell_self_target_only('ride the wind', fly).

elemental_discipline_option('river of hungry flame') :- monk('four elements') >: 17.
elemental_discipline_spell('river of hungry flame', 'wall of fire', 5).

elemental_discipline_option('rush of the gale spirits').
elemental_discipline_spell('rush of the gale spirits', 'gust of wind', 2).

elemental_discipline_option('shape the flowing river').

elemental_discipline_option('sweeping cinder strike').
elemental_discipline_spell('sweeping cinder strike', 'burning hands', 2).

elemental_discipline_option('water whip').

elemental_discipline_option('wave of rolling earth') :- monk('four elements') >: 17.
elemental_discipline_spell('wave of rolling earth', 'wall of stone', 6).


% ------------------------------------------------------------------------------

'elemental discipline'('breath of winter') ?= "You can spend 6 ki points to cast Cone of Cold.".

'elemental discipline'('clench of the north wind') ?= "You can spend 3 ki points to cast Hold Person.".

'elemental discipline'('elemental attunement') ?= "You can use your action to briefly control elemental forces within 30 feet of you, causing one of the following effects of your choice: Create a harmless, instantaneous sensory effect related to air, earth, fire, or water such as a shower of sparks, a puff of wind, a spray of light mist, or a gentle rumbling of stone. Instantaneously light or snuff out a candle, a torch, or a small campfire. Chill or warm up to 1 pound of nonliving material for up to 1 hour. Cause earth, fire, water, or mist that can fit within a 1-foot cube to shape itself into a crude form you designate for 1 minute.".

'elemental discipline'('eternal mountain defense') ?= "You can spend 5 ki points to cast Stoneskin, targeting yourself.".

'elemental discipline'('flames of the phoenix') ?= "You can spend 4 ki points to cast Fireball.".

'elemental discipline'('fangs of the fire snake') ?= "When you use the Attack action on your turn, you can spend 1 ki point to cause tendrils of flame to stretch out from your fists and feet. Your reach with your unarmed strikes increases by 10 feet for that action, as well as the rest of the turn. A hit with such an attack deals fire damage instead of bludgeoning damage, and if you spend 1 ki point when the attack hits, it also deals an extra 1d10 fire damage.".

'elemental discipline'('fist of four thunders') ?= "You can spend 2 ki points to cast Thunderwave.".

'elemental discipline'('fist of unbroken air') ?= "You can create a blast of compressed air that strikes like a mighty fist. As an action, you can spend 2 ki points and choose a creature within 30 feet of you. That creature must make a Strength saving throw. On a failed save, the creature takes 3d10 bludgeoning damage, plus an extra 1d10 bludgeoning damage for each additional ki point you spend, and you can push the creature up to 20 feet away from you and knock it prone. On a successful save, the creature takes half as much damage, and you don't push it or knock it prone.".

'elemental discipline'('gong of the summit') ?= "You can spend 3 ki points to cast Shatter.".

'elemental discipline'('mist stance') ?= "You can spend 4 ki points to cast Gaseous Form, targeting yourself.".

'elemental discipline'('ride the wind') ?= "You can spend 4 ki points to cast Fly, targeting yourself.".

'elemental discipline'('river of hungry flame') ?= "You can spend 5 ki points to cast Wall of Fire.".

'elemental discipline'('rush of the gale spirits') ?= "You can spend 2 ki points to cast Gust of Wind.".

'elemental discipline'('shape the flowing river') ?= "As an action, you can spend 1 ki point to choose an area of ice or water no larger than 30 feet on a side within 120 feet of you. You can change water to ice within the area and vice versa, and you can reshape ice in the area in any manner you choose. You can raise or lower the ice's elevation, create or fill in a trench, erect or flatten a wall, or form a pillar. The extent of any such changes can't exceed half the area's largest dimension. For example, if you affect a 30-foot square, you can create a pillar up to 15 feet high, raise or lower the square's elevation by up to 15 feet, dig a trench up to 15 feet deep, and so on. You can't shape the ice to trap or injure a creature in the area.".

'elemental discipline'('sweeping cinder strike') ?= "You can spend 2 ki points to cast Burning Hands.".

'elemental discipline'('water whip') ?= "You can spend 2 ki points as an action to create a whip of water that shoves and pulls a creature to unbalance it. A creature that you can see that is within 30 feet of you must make a Dexterity saving throw. On a failed save, the creature takes 3d10 bludgeoning damage, plus an extra 1d10 bludgeoning damage for each additional ki point you spend, and you can either knock it prone or pull it up to 25 feet closer to you. On a successful save, the creature takes half as much damage, and you don't pull it or knock it prone.".

'elemental discipline'('wave of rolling earth') ?= "You can spend 6 ki points to cast Wall of Stone.".

