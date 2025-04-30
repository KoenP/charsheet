
%! initialize_new_character(+CharName, -Uuid)
%
%  Create a new character, assigning it a UUID, and initializing a file for it
%  in the character directory.
%  If a user is logged in, additionally call claim_character/1 for that user.
initialize_new_character(CharName, Uuid) :-
    uuid(Uuid),
    initialize_character_file(CharName, Uuid),
    (http_in_session(_) -> claim_character(Uuid) ; true).

initialize_character_file(CharName, Uuid) :-
    mk_path([character_dir, pl(Uuid)], Path),
    open(Path, write, Out),


%! claim_character(+CharName, +Uuid)
%
%  Claim the character with the given identifier for the currently logged in user.
claim_character(Uuid) :-
    user_owned_characters_file_path(Path),
    open(Path, append, Out),
    writeln(Out, Uuid).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% File and directory paths.

% Storage file structure.
%
% character/${uuid}.pl: File that stores all facts related to a character.
% user/${username}/characters: Maps names of characters owned by the account onto uuids.
%
% NEVERMIND: I just need to tweak the old system so the file names are unique IDs rather than char name hashes, so multiple characters with the same name can exist within an account (necessary for public account).
%

logged_in_username(UserName) :-
    http_in_session(_),
    http_session_data(logged_in_as(UserName)).

user_dir(Path) :-
    logged_in_username(UserName),
    format(string(Path), "user/~w", [UserName]).

user_owned_characters_file_path(Path) :-
    mk_path([user_dir, "owned_characters"], Path).

character_dir("character").

pl(Atom, AtomPl) :-
    atomic_list_concat([Atom, ".pl"], AtomPl).

mk_path(Goals, Path) :-
    maplist(call, Goals, Parts),
    intersperse("/", Parts, ToConcat),
    atomic_list_concat(ToConcat, Path).
