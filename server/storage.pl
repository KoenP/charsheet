:- use_module(library(crypto)).

% Declarations.
:- multifile
       name/1,
       base_ability/2,
       gain_level/3,
       choice/3.

:- dynamic name/1, base_ability/2, choice/3, asserted_has/1, store/2.

character_definition_predicate(name/1).
character_definition_predicate(base_ability/2).
character_definition_predicate(choice/3).
character_definition_predicate(asserted_has/1).
character_definition_predicate(store/2).

%! store(?Id, ?Value)
%
%  Store of arbitrary records; typically data that the frontend needs to know
%  about but which has no logical meaning to the backend
%  (e.g. card color config)
store(_,_) :- false.

%! user_id(Id)
%
%  If logged in, the hash of the user name.
%  If not logged in, the atom `public`.
user_id(Hash) :-
    http_in_session(_),
    hashed_user_name(Hash).
user_id(public).

% Hashing predicates.
hashed_user_name(Hash) :-
    http_session_data(logged_in_as(UserName)),
    hash_name(UserName, Hash).

hash_name(Name, Hash) :-
    crypto_data_hash(Name, Hash, [algorithm(md5)]).

character_file_path(CharId, Path) :-
    user_id(UserId),
    format(string(Path), 'storage/~w/~w.pl', [UserId, CharId]).

ensure_user_dir :-
    user_id(Id),
    format(string(Path), 'storage/~w', [Id]),
    (\+ exists_directory(Path) -> make_directory(Path) ; true).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Character operations that modify the character in a snapshot.
with_new_character(CharName, CharId, Cont) :-
    uuid(CharId),
    character_file_path(CharId, Path),
    \+ exists_file(Path),
    ensure_user_dir,
    snapshot(
        (initialize_new_character(CharName),
         (rewrite_character_file(Path), call(Cont)))).

initialize_new_character(Name) :-
    assert(name(Name)),
    forall(ability(Abi), assert(base_ability(Abi,10))).

with_loaded_character(CharId, Cont) :-
    character_file_path(CharId, Path),
    snapshot((load_character_file(Path), call(Cont))).

set_base_abilities(CharId, Abilities) :-
    character_file_path(CharId, Path),
    snapshot(
        (load_character_file(Path),
         forall(member(Abi=Score, Abilities),
                (retractall(base_ability(Abi,_)), assert(base_ability(Abi, Score)))),
         rewrite_character_file(Path))).

withdraw_gain_level(CharId, RetractedLevel) :-
    character_file_path(CharId, Path),
    snapshot(
        (load_character_file(Path),
         level(CurLevel),
         forall(between(RetractedLevel, CurLevel, L), retractall(choice(level(L),_,_))),
         resolve_ineligible_choices,
         rewrite_character_file(Path))).
withdraw_character_fact(CharId, Fact) :-
    character_file_path(CharId, Path),
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

withdraw_character_fact_without_resolving(CharId, Fact) :-
    character_file_path(CharId, Path),
    snapshot(
        (load_character_file(Path),
         ( retract(Fact),
           retractall(Fact),
           rewrite_character_file(Path)
         ; true))).

record_character_fact(CharId, Fact) :-
    character_file_path(CharId, Path),
    append_to_character_file(Path, Fact).

list_characters(IdNamePairs) :-
    user_id(UserId),
    format(string(Pattern), 'storage/~w/*.pl', [UserId]),
    expand_file_name(Pattern, FilePaths),
    findall(CharId-Name,
            (member(Path, FilePaths),
             extract_id_from_file_path(Path, CharId),
             character_file_peek_name(Path, Name)),
            IdNamePairs).

extract_id_from_file_path(Path, Id) :-
    atom_codes(Path, Codes),
    append([`storage/`, _, `/`, IdCodes, `.pl`], Codes),
    atom_codes(Id, IdCodes).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Character file operations.
character_file_peek_name(Path, Name) :-
    exists_file(Path),
    open(Path, read, In),
    repeat,
    catch(read_term(In, Term, []), _, true),
    (Term = name(Name) ; Term = end_of_file), !,
    ground(Name).

load_character_file(Path) :-
    abolish_private_tables,
    exists_file(Path),
    !,
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
    exists_file(Path),
    open(Path, append, Out),
    write_term(Out, Fact, [quoted(true), fullstop(true), nl(true)]),
    close(Out).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% For easy testing and debugging.

:- op(1200, xfx, ::).

(CharName :: Goal) :-
    hash_name(koen, UserNameHash),
    hash_name(CharName, CharNameHash),
    format(string(Path), 'storage/~w/characters/~w.pl', [UserNameHash, CharNameHash]),
    snapshot((load_character_file(Path), call(Goal))).
