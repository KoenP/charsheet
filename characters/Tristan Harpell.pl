name('Tristan Harpell').
base_ability(dex,12).
base_ability(con,12).
base_ability(wis,12).
base_ability(cha,12).
base_ability(int,18).
base_ability(str,16).
gain_level(2,wizard,hp_avg).
gain_level(3,wizard,hp_avg).
gain_level(4,wizard,hp_avg).
gain_level(5,wizard,hp_avg).
gain_level(6,wizard,hp_avg).
gain_level(7,wizard,hp_avg).
gain_level(8,wizard,hp_avg).
gain_level(9,wizard,hp_avg).
gain_level(10,wizard,hp_avg).
gain_level(11,wizard,hp_avg).
gain_level(12,wizard,hp_avg).
gain_level(13,wizard,hp_avg).
gain_level(14,wizard,hp_avg).
gain_level(15,wizard,hp_avg).
gain_level(16,wizard,hp_avg).
gain_level(17,wizard,hp_avg).
gain_level(18,wizard,hp_avg).
gain_level(19,warlock,hp_avg).
gain_level(20,warlock,hp_avg).
choice(init,'base race',human).
choice(race(human),subrace,variant).
choice(race(human(variant)),skill,history).
choice(race(human(variant)),asi,[int+1,con+1]).
choice(init,'initial class',wizard).
choice(wizard >: 2,subclass,evocation).
%choice(wizard >: 2,'free spell',['burning hands','comprehend languages']).
%choice(wizard >: 3,'free spell',[shatter,'see invisibility']).
%choice(wizard >: 4,cantrip,'fire bolt').
%%choice(wizard >: 4,'free spell',[darkvision,'misty step']).
%%choice(wizard >: 5,'free spell',[counterspell,fireball]).
%%choice(wizard >: 6,'free spell',[slow,'stinking cloud']).
%%choice(warlock >: 1,cantrip,['eldritch blast','blade ward']).
%choice(warlock >: 2,'eldritch invocation',['mask of many faces','agonizing blast']).
%choice(wizard >: 18,spell_mastery(1),thunderwave).
%choice(wizard >: 18,spell_mastery(2),shatter).
%choice(wizard >: 4,'asi or feat',alert).
%choice(wizard >: 1,cantrip,['blade ward','dancing lights',message]).
%%choice(wizard >: 1,'free spell',['abi-dalzim\'s horrid wilting','aganazzar\'s scorcher','arcane gate']).
%choice(initial_class(wizard),skill,[medicine]).
%choice(wizard >: 16,'free spell',['delayed blast fireball','antipathy/sympathy']).
%choice(wizard >: 16,'asi or feat',[cha]).
%
%choice(wizard >: 16,'free spell',['glyph of warding','prismatic wall']).

known_spell(wizard, int, 'when prepared', [], no, Name) :-
    spell_data(Name,_).
    %member(Name,
    %       [ 'prismatic wall',
    %         confusion,
    %         'control weather',
    %         creation,
    %         reincarnate,
    %         scrying,
    %         symbol,
    %         teleport,
    %         imprisonment,
    %         'control water',
    %         wish,
    %         'glyph of warding',
    %         'antimagic field',
    %         'true polymorph',
    %         hallow,
    %         'animate objects',
    %         'guards and wards',
    %         shapechange,
    %         'antipathy/sympathy',
    %         'magic jar',
    %         'planar ally',
    %         'arcane hand',
    %         'astral projection',
    %         earthquake,
    %         'find familiar',
    %         'detect thoughts',
    %         'prismatic spray',
    %         contagion,
    %         'fire bolt'
    %       ]
    %      ).

