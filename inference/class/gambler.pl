class_option(gambler).
hd_per_level(gambler, 1 d 8).
initial_class_base_hp(gambler, 8).
max_hp_per_level(gambler, 1 d 8).
caster(gambler, 0).
%spellcasting_ability(gambler, cha).
%choose_subclass_level(gambler:1).
asi_level(gambler:L) :-
    default_asi_level(L).
class_saving_throw(gambler, dex).
class_saving_throw(gambler, cha).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Features gained for picking this class as initial class.
trait_options_source(initial_class(gambler), skill, wrap(skill),
                     3 unique_from from_list(
                         [acrobatics, athletics, deception, insight,
                          investigation, perception, performance,
                          persuasion, 'sleight of hand', stealth])).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Features from leveling up.
traits_from_source(gambler >: 1,
                   [armor(light), weapon(simple),
                    weapon('hand crossbow'), weapon(rapier),
                    tool('deck of cards'), tool(dice)]).

trait_source(gambler >: 1, card_throwing(N)) :-
    class_level(gambler:L),
    ordered_lookup_largest_leq([1->1, 5->2, 11->3, 17->4], L, N).

attack('throw card', feet(20)/feet(40), to_hit(ToHit), [damage(piercing,1)], Notes) :-
    trait(card_throwing(NCards)),
    ability_mod(dex, DexMod),
    proficiency_bonus(ProfBon),
    ToHit is DexMod + ProfBon,
    format(string(NCardsNote), 'throw ~d cards', NCards),
    (NCards == 1 -> Notes = [] ; Notes = [NCardsNote]).

trait_source(gambler >: 1, 'deck infusion').
trait_source(gambler >: 1, 'basic deck infusion').
trait_source(gambler >: 1, 'draw hand').

meta_todo('enhanced deck infusion', 'need to implement gambler spellcasting').

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

card_throwing(_) ?= "You have learned yourself to throw cards in a harmful manner. As an action you can draw and throw a card. Roll a d20 and add your Dexterity modifier and your proficiency bonus to the roll to try and hit a target. A deck of cards has a short range of 20 feet and a long range of 40 feet. On a hit with a mundane deck of cards, you deal 1 piercing damage.".

'deck infusion' ?= "Your obsession with gambling has even interested the Gods of Fate and Luck. Just for their own pleasure they have given you the power to infuse your deck of cards with magical powers.
Once per long rest you can infuse a deck of cards, making it capable of dealing magical damage instead of the 1 piercing damage. The infusion requires a deck of a number of cards depending on the type of deck, which is completely shuffled in a random order. Once infused, the cards become completely blank. Only when drawn, the original content of the card becomes clear.".

'basic deck infusion' ?= "Starting at 1st level you gain the power to infuse a basic deck of 54 cards (including jokers) with magical powers. The power of the card is dependent on its contents. The suit of the deck determines the type of damage, and the value determines the damage. Jack, Queen, King and Jokers have specific effects of their own. Kings have strong effects on the enemy, Jacks have the same effect but on the gambler. A target must be chosen before drawing a card and throwing it.
At higher levels, your Card Throwing ability grows and you are able to throw more cards at once at possibly multiple targets. At 5th level you can throw two cards. This amount increases by one at 11th level and once more at 17th level.

Hearts: Fire Damage
Diamonds: Cold Damage
Clubs: Lightning Damage
Spades: Acid Damage
Jack of Hearts: Wis Save or become Charmed
Jack of Diamonds: Con Save or become Incapacitated
Jack of Clubs: Con Save or become Paralysed
Jack of Spades Con Save or become Poisoned
Queen of Hearts: Target gets disadvantage on next attack
Queen of Diamonds: Targets movement halved next turn
Queen of Clubs: Targets reaction lost
Queen of Spades Acid splash cantrip on target
King of Hearts: Wis Save or target Charmed
King of Diamonds: Con Save or target Incapacitated
King of Clubs: Con Save or target Paralysed
King of Spades Con Save or target Poisoned
Joker (2): See Joker feature in Deck Building".

'draw hand' ?= "At 1st level as a bonus action you can spend one Gambler Point to draw a hand of three cards from one deck. You must keep these cards in your hands to be able to play the cards. You cannot draw new cards from that deck until you have used or burned all cards in the drawn hand.".
