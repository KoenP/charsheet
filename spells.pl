:- discontiguous
       spell/2.

% Cantrips.
spell('fire bolt',
      properties(
          school: evocation,
          level: 0,
          casting_time: action,
          range: 120,
          components: [v, s],
          duration: instantaneous)).

spell('druidcraft',
      properties(
          school:transmutation,
          level: 0,
          casting_time: action,
          range: 30,
          components: [v, s],
          duration: instantaneous,
          info: "Whispering to the spirits of nature, you create one of the following effects within range: * You create a tiny, harmless sensory effect that predicts what the weather will be at your location for the next 24 hours. The effect might manifest as a golden orb for clear skies, a cloud for rain, falling snowflakes for snow, and so on. This effect persists for 1 round. * You instantly make a flower blossom, a seed pod open, or a leaf bud bloom. * You create an instantaneous, harmless sensory effect, such as falling leaves, a puff of wind, the sound of a small animal, or the faint odor of skunk. The effect must fit in a 5-foot cube. * You instantly light or snuff out a candle, a torch, or a small campfire."
      )).




spell('shillelagh',
      properties(
          school: transmutation,
          level: 0,
          casting_time: bonus,
          range: touch,
          components: [v, s, m("mistletoe, a shamrock leaf, and a club or quarterstaff")],
          duration: minutes(1),
          info: "The wood of a club or quarterstaff you are holding is imbued with nature's power. For the duration, you can use your spellcasting ability instead of Strength for the attack and damage rolls of melee attacks using that weapon, and the weapon's damage die becomes a d8. The weapon also becomes magical, if it isn't already. The spell ends if you cast it again or if you let go of the weapon."
      )).

spell('guidance',
      properties(
          school: divination,
          level: 0,
          casting_time: action,
          range: touch,
          components: [v, s],
          duration: concentration(minutes(1)))).
spell('guidance') ?= "You touch one willing creature. Once before the spell ends, the target can roll a d4 and add the number rolled to one ability check of its choice. It can roll the die before or after making the ability check. The spell then ends.".

% Level 1
spell('healing word',
      properties(
          school: evocation,
          level: 1,
          casting_time: bonus,
          range: 60,
          components: [v],
          duration: instantaneous)).

% Level 2
spell('moonbeam',
      properties(
          school: evocation,
          level: 2,
          casting_time: action,
          range: 120,
          components: [v,s,m("several seeds of any moonseed plant and a piece of opalescent feldspar")],
          duration: concentration(minutes(1)))).
