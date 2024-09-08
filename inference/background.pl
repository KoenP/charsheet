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
% Entertainer.
background_option(entertainer).
traits_from_source(background(entertainer),
                   [skill(acrobatics), skill(performance), tool('disguise kit')]).
trait_options_source(background(entertainer), 'musical instrument', wrap(tool),
                     musical_instrument).

trait_source(background(entertainer), 'by popular demand').
'by popular demand' ?= "You can always find a place to perform, usually in an inn or tavern but possibly with a circus, at a theater, or even in a noble’s court. At such a place, you receive free lodging and food of a modest or comfortable standard (depending on the quality of the establishment), as long as you perform each night. In addition, your performance makes you something of a local figure. When strangers recognize you in a town where you have performed, they typically take a liking to you.".

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Sage.
background_option(sage).
traits_from_source(background(sage), [skill(arcana), skill(history)]).
trait_options_source(background(sage), language, 2 unique_from language).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Archaeologist (NOT SRD!).
background_option(archaeologist).
traits_from_source(background(archaeologist),
                   [skill(history), skill(survival),
                    tool(cartographer), tool(navigator),
                    'historical knowledge']).
trait_options_source(background(archaeologist), language, wrap(language), language).

'historical knowledge' ?= "Can determine purpose and origin of ruin or dungeon. Can determine value of old objects.".

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Outlander (NOT SRD!).
background_option(outlander).
traits_from_source(background(outlander), [skill(athletics), skill(survival), wanderer]).
trait_options_source(background(outlander), language, wrap(language), language).
meta_todo(background(outlander), "plays a musical instrument").
wanderer ?= "You have an excellent memory for maps and geography, and you can always recall the general layout of terrain, settlements, and other features aorund you. In addition, you can find food and fresh water for yourself and up to five other people each day, provided that the land offers berries, small game, water, and so forth.".
