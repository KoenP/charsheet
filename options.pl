:- multifile
       pick_trait/3. % TODO needs a rename... It's fundamentally different from pick_feat, pick_abi, ...

trait(pick_trait(Condition, Name), Trait) :-
    valid_pick_trait(Condition, Name, Trait).

valid_pick_trait(Condition, Name, Selection) :-
    pick_trait(Condition, Name, Selection),
    \+ problem(pick_trait(Condition, Name), _).

% PC doesn't match the criteria to make a pick.
problem(pick_trait(Condition, Name), not_eligible) :-
    pick_trait(Condition, Name, _),
    \+ trait_options(Condition, Name, _, _).

% User didn't pick the correct number of options.
problem(pick_trait(Condition, Name), (should_pick(NOptions), have_picked(NPicks))) :-
    trait_options(Condition, Name, NOptions, _),
    findall(Pick, pick_trait(Condition, Name, Pick), Picks),
    length(Picks, NPicks),
    NPicks \= NOptions.

% User picked a nonexistant option, without specific error message.
problem(pick_trait(Condition, Name), invalid_option(Selection)) :-
    pick_trait(Condition, Name, Selection),
    \+ trait_options(Condition, Name, _, Selection),
    \+ trait_bad_options(Condition, Name, Selection, _),
    trait_options(Condition, Name, _, _).

% User picked a nonexistant option, but for which we have a more specific error message.
problem(pick_trait(Condition, Name), bad_choice(Selection, ErrMsg)) :-
    pick_trait(Condition, Name, Selection),
    trait_bad_options(Condition, Name, Selection, ErrMsg).

% User picked multiple times for the same choice.
problem(pick_trait(Condition, Name), picked_more_than_once) :-
    trait_options(Condition, Name, _, _),
    findall(Pick, pick_trait(Condition, Name, Pick), [_,_|_]).

% Generate a todo when the user has not yet picked any options for a trait they are eligible for.
todo(no_option_picked(Condition, Name)) :-
    trait_options(Condition, Name, _, _),
    \+ pick_trait(Condition, Name, _).






% example pick_trait use cases
:- op(650, xfx, from).

known_spells_at(_, X) :-
    member(X, [disguise_self, magic_missile, featherfall]).

learnable_spells(X) :-
    member(X, [fireball, lightning_bolt]).

trait_options(class(sorcerer:Level), spell_replace, 2 from [1 from Forget, 1 from Learn]) :-
    Level1 is Level - 1,
    findall(Spell, known_spells_at(Level1, Spell), Forget),
    findall(Spell, learnable_spells(Spell), Learn),
    between(1, 20, Level).

trait_options(level(4), test, 1 from [1 from [2 from AbiPlusOne, 1 from AbiPlusTwo], 1 from Feats]) :-
    findall(Ability+1, ability(Ability), AbiPlusOne),
    findall(Ability+2, ability(Ability), AbiPlusTwo),
    findall(feat(Feat), feat_option(Feat), Feats).

%fulfilled(Origin, Name) :-
%    trait_options(Origin, Name, Options)


%choice(class(sorcerer:Level), replace_spell, 2 from [forget, choose_new]) :-
%    between(2, 20, Level).
%option(class(sorcerer:Level), replace_spell:forget, 1, )


option(level(4), levelup, 1 from [ability_score_increase, feat]).

option(level(4), levelup/ability_score_increase, 1 from [2 from two_abilities, 1 from one_ability]).
option(level(4), levelup/ability_score_increase/two_abilities, Ability+1) :-
    ability(Ability).
option(level(4), levelup/ability_score_increase/one_ability, Ability+2) :-
    ability(Ability).

option(level(4), levelup/feat, feat(Feat)) :-
    feat_option(Feat).

%pick(level(4), levelup/ability_score_increase/two_abilities, str+1).
%pick(level(4), levelup/ability_score_increase/two_abilities, dex+1).

pick(level(4), levelup/ability_score_increase/one_ability, dex+1).

pick(level(4), levelup/feat, feat(alert)).


count(X, L, N) :-
    findall(_, member(X,L), Xs),
    length(Xs, N).

status(N, Statuses, fulfilled) :-
    count(fulfilled, Statuses, N),
    \+ member(bad, Statuses),
    !.
status(_, Statuses, null) :-
    \+ (member(X, Statuses), (X = bad; X = fulfilled)),
    !.
status(_, _, bad).

option_status(Origin, Name, N from Options, Status) :-
    is_list(Options),
    !,
    findall(Stat, (member(Option, Options), option_status(Origin, Name, Option, Stat)), Statuses),
    status(N, Statuses, Status).
    

ok(Origin, Name) :-
    option(Origin, Name, Options),
    !,
    ok(Origin, Name, Options).
%ok(Origin, Name, N from Options) :-
%    is_list(Options),
%    !,
%    findall(Option, (member(Option, Options), ok(Origin, Name, Option)), Validated),
%    findall(Option, (member(Option, Options), \+ ok(Origin, Name, Option)), NotValidated),
%    length(Validated, N),
%    findall(_, (member(X,NotValidated), ))
    
ok(Origin, Name, Menu) :-
    atom(Menu),
    !,
    ok(Origin, Name, 1 from Menu).
ok(Origin, Name, N from Menu) :-
    atom(Menu),
    !,
    findall(Option, (option(Origin, Name/Menu, Option), ok(Origin, Name/Menu, Option)), Validated),
    length(Validated, N).
ok(Origin, Name, X) :-
    !,
    pick(Origin, Name, X).


% pick_trait()
