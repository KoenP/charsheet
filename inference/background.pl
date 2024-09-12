:- discontiguous background_option/1.

meta_todo(background, "all equipment from background").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Acolyte.
background_option(acolyte).
traits_from_source(background(acolyte),
                   [skill(insight), skill(religion),
                    'shelter of the faithful']).
trait_options_source(background(acolyte), language, wrap(language), language).
'shelter of the faithful' ?= "As an acolyte, you command the respect of those who share your faith, and you can perform the religious ceremonies of your deity. You and your adventuring companions can expect to receive free healing and care at a temple, shrine, or other established presence of your faith, though you must provide any material components needed for spells. Those who share your religion will support you (but only you) at a modest lifestyle. You might also have ties to a specific temple dedicated to your chosen deity or pantheon, and you have a residence there. This could be the temple where you used to serve, if you remain on good terms with it, or a temple where you have found a new home. While near your temple, you can call upon the priests for assistance, provided the assistance you ask for is not hazardous and you remain in good standing with your temple.".

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Charlatan.
background_option(charlatan).
traits_from_source(background(charlatan),
                   [skill(deception), skill('sleight of hand'),
                    tool('disguise kit', tool('forgery kit')),
                    'false identity'
                   ]).
'false identity' ?= "You have created a second identity that includes documentation, established acquaintances, and disguises that allow you to assume that persona. Additionally, you can forge documents including official papers and personal letters, as long as you have seen an example of the kind of document or the handwriting you are trying to copy.".

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Criminal
background_option(criminal).
traits_from_source(background(criminal),
                   [skill(deception), skill(stealth),
                    tool('thieves\' tools'),
                    'criminal contact'
                   ]).
trait_options_source(background(criminal), 'gaming set', wrap(tool), gaming_set).

'criminal contact' ?= "You have a reliable and trustworthy contact who acts as your liaison to a network of other criminals. You know how to get messages to and from your contact, even over great distances; specifically, you know the local messengers, corrupt caravan masters, and seedy sailors who can deliver messages for you.".


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Entertainer.
background_option(entertainer).
traits_from_source(background(entertainer),
                   [skill(acrobatics), skill(performance), tool('disguise kit')]).
trait_options_source(background(entertainer), 'musical instrument', wrap(tool),
                     musical_instrument).

trait_source(background(entertainer), 'by popular demand').
'by popular demand' ?= "You can always find a place to perform, usually in an inn or tavern but possibly with a circus, at a theater, or even in a noble’s court. At such a place, you receive free lodging and food of a modest or comfortable standard (depending on the quality of the establishment), as long as you perform each night. In addition, your performance makes you something of a local figure. When strangers recognize you in a town where you have performed, they typically take a liking to you.".

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Folk hero.
background_option('folk hero').
traits_from_source(background('folk hero'),
                   [skill('animal handling'), skill(survival),
                    tool('disguise kit'), tool('land vehicles'),
                    'rustic hospitality'
                   ]).
trait_options_source(background('folk hero'), 'artisan\s tools', wrap(tool),
                     artisans_tools).

'rustic hospitality' ?= "Since you come from the ranks of the common folk, you fit in among them with ease. You can find a place to hide, rest, or recuperate among other commoners, unless you have shown yourself to be a danger to them. They will shield you from the law or anyone else searching for you, though they will not risk their lives for you.".

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Gladiator
background_option(gladiator).
traits_from_source(background(gladiator),
                   [ skill(acrobatics), skill(performance), tool('disguise kit'),
                     'by popular demand' ]).
trait_options_source(background(gladiator), 'musical instrument', wrap(tool),
                     musical_instrument).

'by popular demand' ?= "You can always find a place to perform in any place that features combat for entertainment — perhaps a gladiatorial arena or secret pit fighting club. At such a place, you receive free lodging and food of a modest or comfortable standard (depending on the quality of the establishment), as long as you perform each night. In addition, your performance makes you something of a local figure. When strangers recognize you in a town where you have performed, they typically take a liking to you.".

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Guild artisan
background_option('guild artisan').
traits_from_source(background('guild artisan'),
                   [skill(insight), skill(persuasion)]).
trait_options_source(background('guild artisan'), 'artisan\'s tools', wrap(tool),
                     artisans_tools).
trait_options_source(background('guild artisan'), 'language', wrap(language),
                     language).

'guild membership' ?= "As an established and respected member of a guild, you can rely on certain benefits that membership provides. Your fellow guild members will provide you with lodging and food if necessary, and pay for your funeral if needed. In some cities and towns, a guildhall offers a central place to meet other members of your profession, which can be a good place to meet potential patrons, allies, or hirelings.

Guilds often wield tremendous political power. If you are accused of a crime, your guild will support you if a good case can be made for your innocence or the crime is justifiable. You can also gain access to powerful political figures through the guild, if you are a member in good standing. Such connections might require the donation of money or magic items to the guild’s coffers.

You must pay dues of 5 gp per month to the guild. If you miss payments, you must make up back dues to remain in the guild’s good graces.".

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Hermit.
background_option(hermit).
traits_from_source(background(hermit),
                   [skill(medicine), skill(religion), tool('herbalism kit')]).
trait_options_source(background(hermit), language, wrap(language), language).

discovery ?= "The quiet seclusion of your extended hermitage gave you access to a unique and powerful discovery. The exact nature of this revelation depends on the nature of your seclusion. It might be a great truth about the cosmos, the deities, the powerful beings of the outer planes, or the forces of nature. It could be a site that no one else has ever seen. You might have uncovered a fact that has long been forgotten, or unearthed some relic of the past that could rewrite history. It might be information that would be damaging to the people who consigned you to exile, and hence the reason for your return to society.

Work with your DM to determine the details of your discovery and its impact on the campaign.".

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Knight.
background_option(knight).
traits_from_source(background(knight), [skill(history), skill(persuasion), retainers]).
trait_options_source(background(knight), 'gaming set', wrap(tool), gaming_set).
trait_options_source(background(knight), language, wrap(language), language).

retainers ?= "You have the service of three retainers loyal to your family. These retainers can be attendants or messengers, and one might be a majordomo. Your retainers are commoners who can perform mundane tasks for you, but they do not fight for you, will not follow you into obviously dangerous areas (such as dungeons), and will leave if they are frequently endangered or abused.".

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Noble.
background_option(noble).
traits_from_source(background(noble), [skill(history), skill(persuasion)]).
trait_options_source(background(noble), 'gaming set', wrap(tool), gaming_set).
trait_options_source(background(noble), language, wrap(language), language).

'position of privilege' ?= "Thanks to your noble birth, people are inclined to think the best of you. You are welcome in high society, and people assume you have the right to be wherever you are. The common folk make every effort to accommodate you and avoid your displeasure, and other people of high birth treat you as a member of the same social sphere. You can secure an audience with a local noble if you need to.".

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Outlander.
background_option(outlander).
traits_from_source(background(outlander),
                   [skill(athletics), skill(survival), wanderer]).
trait_options_source(background(outlander), 'musical instrument', wrap(tool),
                     musical_instrument).
trait_options_source(background(outlander), language, wrap(language), language).

wanderer ?= "You have an excellent memory for maps and geography, and you can always recall the general layout of terrain, settlements, and other features around you. In addition, you can find food and fresh water for yourself and up to five other people each day, provided that the land offers berries, small game, water, and so forth.".

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Pirate.
background_option(pirate).
traits_from_source(background(pirate),
                   [skill(athletics), skill(perception), tool('navigator\'s tools'),
                    'water vehicles', 'bad reputation']).

'bad reputation' ?= "No matter where you go, people are afraid of you due to your reputation. When you are in a settlement, you can get away with minor criminal offenses, such as refusing to pay for food at a tavern or breaking down doors at a local shop, since most people will not report your activity to the authorities.".

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Sage.
background_option(sage).
traits_from_source(background(sage), [skill(arcana), skill(history)]).
trait_options_source(background(sage), language, wrap(language), 2 unique_from language).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Sailor.
background_option(sailor).
traits_from_source(background(sailor), 
                   [skill(athletics), skill(perception), tool('navigator\'s tools'),
                    'water vehicles', 'ship\'s passage']).

'ship\'s passage' ?= "When you need to, you can secure free passage on a sailing ship for yourself and your adventuring companions. You might sail on the ship you served on, or another ship you have good relations with (perhaps one captained by a former crewmate). Because you’re calling in a favor, you can’t be certain of a schedule or route that will meet your every need. Your Dungeon Master will determine how long it takes to get where you need to go. In return for your free passage, you and your companions are expected to assist the crew during the voyage.".

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Soldier.
background_option(soldier).
traits_from_source(background(soldier), 
                   [skill(athletics), skill(intimidation), 'land vehicles',
                    'military rank']).
trait_options_source(background(soldier), 'gaming set', wrap(tool), gaming_set).

'military rank' ?= "You have a military rank from your career as a soldier. Soldiers loyal to your former military organization still recognize your authority and influence, and they defer to you if they are of a lower rank. You can invoke your rank to exert influence over other soldiers and requisition simple equipment or horses for temporary use. You can also usually gain access to friendly military encampments and fortresses where your rank is recognized.".

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Urchin.
background_option(urchin).
traits_from_source(background(urchin),
                   [ skill('sleight of hand'), skill(stealth),
                     tool('disguise kit'), tool('thieves\' tools'),
                     'city secrets' ]).

'city secrets' ?= "You know the secret patterns and flow to cities and can find passages through the urban sprawl that others would miss. When you are not in combat, you (and companions you lead) can travel between any two locations in the city twice as fast as your speed would normally allow.".


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Archaeologist (NOT PHB!).
background_option(archaeologist).
traits_from_source(background(archaeologist),
                   [skill(history), skill(survival),
                    tool(cartographer), tool(navigator),
                    'historical knowledge']).
trait_options_source(background(archaeologist), language, wrap(language), language).

'historical knowledge' ?= "Can determine purpose and origin of ruin or dungeon. Can determine value of old objects.".

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Outlander (NOT PHB!).
background_option(outlander).
traits_from_source(background(outlander), [skill(athletics), skill(survival), wanderer]).
trait_options_source(background(outlander), language, wrap(language), language).
meta_todo(background(outlander), "plays a musical instrument").
wanderer ?= "You have an excellent memory for maps and geography, and you can always recall the general layout of terrain, settlements, and other features aorund you. In addition, you can find food and fresh water for yourself and up to five other people each day, provided that the land offers berries, small game, water, and so forth.".
