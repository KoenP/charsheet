:- discontiguous
       background_option/1,
       background_trait/2,
       background_trait_options/3.

todo(background) :-
    \+ background(_).

problem(background, background_does_not_exist(B)) :-
    background(B),
    \+ background_option(B).
problem(background, picked_multiple_backgrounds(Bs)) :-
    findall(B, background(B), Bs),
    Bs = [_,_|_].

gain_trait(1, background(B), T) :-
    background(B),
    background_trait(B, T).

trait_options(background(B), Name, Spec) :-
    background(B),
    background_trait_options(B, Name, Spec).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
background_option(acolyte).
background_trait(acolyte, skill(insight)).
background_trait(acolyte, skill(religion)).
background_trait_options(acolyte, language, 2 from [language(_), language(_)]).
background_trait(acolyte, 'shelter of the faithful').
'shelter of the faithful' ?= "As an acolyte, you command the respect of those who share your faith, and you can perform the religious ceremonies of your deity. You and your adventuring companions can expect to receive free healing and care at a temple, shrine, or other established presence of your faith, though you must provide any material components needed for spells. Those who share your religion will support you (but only you) at a modest lifestyle.

You might also have ties to a specific temple dedicated to your chosen deity or pantheon, and you have a residence there. This could be the temple where you used to serve, if you remain on good terms with it, or a temple where you have found a new home. While near your temple, you can call upon the priests for assistance, provided the assistance you ask for is not hazardous and you remain in good standing with your temple.".

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
background_option(hermit).
background_trait(hermit, skill(medicine)).
background_trait(hermit, skill(religion)).
background_trait(hermit, tool('herbalism kit')).
background_trait(hermit, discovery). % TODO
background_trait_options(hermit, language, 1 from [language(_)]). % TODO

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
background_option(noble).
background_trait(noble, skill(history)).
background_trait(noble, skill(persuasion)).
background_trait_options(noble, gaming_set, 1 from [tool(_)]). % TODO
background_trait_options(noble, language, 1 from [language(_)]). % TODO
background_trait(noble, 'position of privilege').
'position of privilege' ?= "Thanks to your noble birth, people are inclined to think the best of you. You are welcome in high society, and people assume you have the right to be wherever you are. The common folk make every effort to accommodate you and avoid your displeasure, and other people of high birth treat you as a member of the same social sphere. You can secure an audience with a local noble if you need to.".
