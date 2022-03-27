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
% Archaeologist (NOT SRD!).
background_option(archaeologist).
traits_from_source(background(archaeologist),
                   [skill(history), skill(survival),
                    tool(cartographer), tool(navigator),
                    'historical knowledge']).
trait_options_source(background(archaeologist), language, wrap(language), language).

'historical knowledge' ?= "Can determine purpose and origin of ruin or dungeon. Can determine value of old objects.".
