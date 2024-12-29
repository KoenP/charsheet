:- use_module(library(crypto)).

% Declarations.
:- multifile
       name/1,
       base_ability/2,
       gain_level/3,
       choice/3.

:- dynamic name/1, base_ability/2, gain_level/3, choice/3.

character_definition_predicate(name/1).
character_definition_predicate(base_ability/2).
character_definition_predicate(gain_level/3).
character_definition_predicate(choice/3).
character_definition_predicate(has/1).

% Hashing predicates.
hashed_user_name(Hash) :-
    http_session_data(logged_in_as(UserName)),
    hash_name(UserName, Hash).

hash_name(Name, Hash) :-
    crypto_data_hash(Name, Hash, [algorithm(md5)]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Character operations that modify the character in a snapshot.
with_new_character(CharName, Cont) :-
    character_file_path(CharName, Path),
    \+ exists_file(Path),
    snapshot(
        (initialize_new_character(CharName),
         character_dir_path(DirPath),
         make_directory_path(DirPath),
         (rewrite_character_file(Path), call(Cont)))).

initialize_new_character(Name) :-
    assert(name(Name)),
    forall(ability(Abi), assert(base_ability(Abi,10))).

with_loaded_character(CharName, Cont) :-
    character_file_path(CharName, Path),
    snapshot((load_character_file(Path), call(Cont))).

set_base_abilities(CharName, Abilities) :-
    character_file_path(CharName, Path),
    snapshot(
        (load_character_file(Path),
         forall(member(Abi=Score, Abilities),
                (retractall(base_ability(Abi,_)), assert(base_ability(Abi, Score)))),
         rewrite_character_file(Path))).

withdraw_gain_level(CharName, RetractedLevel) :-
    character_file_path(CharName, Path),
    snapshot(
        (load_character_file(Path),
         level(CurLevel),
         forall(between(RetractedLevel, CurLevel, L), retractall(gain_level(L, _, _))),
         resolve_ineligible_choices,
         rewrite_character_file(Path))).
withdraw_character_fact(CharName, Fact) :-
    character_file_path(CharName, Path),
    snapshot(
        (load_character_file(Path),
         ( retract(Fact),
           retractall(Fact),
           resolve_ineligible_choices,
           rewrite_character_file(Path)
         ; true))).
resolve_ineligible_choices :-
    abolish_private_tables,
    findall(Origin-Id, problem(not_eligible(Origin, Id, _)), ChoicesToUndo),
    forall(member(Origin-Id, ChoicesToUndo), retractall(choice(Origin, Id, _))),
    (ChoicesToUndo \= [] -> resolve_ineligible_choices ; true).

record_character_fact(CharName, Fact) :-
    character_file_path(CharName, Path),
    append_to_character_file(Path, Fact).

list_characters(Names) :-
    hashed_user_name(UserNameHash),
    format(string(Pattern), 'storage/~w/characters/*.pl', [UserNameHash]),
    expand_file_name(Pattern, FilePaths),
    findall(Name,
            (member(Path, FilePaths), character_file_peek_name(Path, Name)),
            Names).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Character file operations.
character_file_peek_name(Path, Name) :-
    open(Path, read, In),
    repeat,
    catch(read_term(In, Term, []), _, true),
    (Term = name(Name) ; Term = end_of_file), !,
    ground(Name).

load_character_file(Path) :-
    abolish_private_tables,
    open(Path, read, In),
    repeat,
    read_term(In, Term, []),
    assert(Term),
    Term = end_of_file, !,
    close(In).
    
rewrite_character_file(Path) :-
    open(Path, write, Out),
    forall((character_definition_predicate(Pred/N),
            length(Args, N),
            Term =.. [Pred | Args],
            call(Term)
           ),
           write_term(Out, Term, [quoted(true), fullstop(true), nl(true)])),
    close(Out).

append_to_character_file(Path, Fact) :-
    open(Path, append, Out),
    write_term(Out, Fact, [quoted(true), fullstop(true), nl(true)]),
    close(Out).

character_file_path(CharName, Path) :-
    character_dir_path(DirPath),
    hash_name(CharName, CharNameHash),
    format(string(Path), '~w/~w.pl', [DirPath, CharNameHash]).

character_dir_path(Path) :-
    user_dir_path(UserPath),
    format(string(Path), '~w/characters', [UserPath]).

user_dir_path(Path) :-
    hashed_user_name(UserNameHash),
    format(string(Path), 'storage/~w', [UserNameHash]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% For easy testing and debugging.

:- op(1200, xfx, ::).

(CharName :: Goal) :-
    hash_name(koen, UserNameHash),
    hash_name(CharName, CharNameHash),
    format(string(Path), 'storage/~w/characters/~w.pl', [UserNameHash, CharNameHash]),
    snapshot((load_character_file(Path), call(Goal))).
