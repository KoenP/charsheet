:- module(accounts, [accounts_file/1,
                     load_accounts/1,
                     save_accounts/1,
                     create_account/2,
                     verify_credentials/2,
                     set_password/2,
                     account_names/1,
                     valid_username/1,
                     valid_password/1]).

:- use_module(library(crypto)).
:- use_module(library(pcre)).

accounts_mutex('accounts_db').

%!  accounts_file(-File)
%
%   Path to the account database. Controlled by the CHARSHEET_ACCOUNTS_FILE
%   environment variable, defaulting to 'storage/accounts.pl'.
accounts_file(File) :-
    (   getenv('CHARSHEET_ACCOUNTS_FILE', File)
    ->  true
    ;   File = 'storage/accounts.pl'
    ).

%!  load_accounts(-Accounts)
%
%   Read the account database into a list of account(Name, Hash, Created)
%   terms. Returns [] when the file is missing. Malformed terms are skipped.
load_accounts(Accounts) :-
    accounts_file(File),
    (   exists_file(File)
    ->  setup_call_cleanup(open(File, read, In),
                           read_accounts_loop(In, Accounts),
                           close(In))
    ;   Accounts = []
    ).

read_accounts_loop(In, Accounts) :-
    catch(read_term(In, Term, []), _, fail),
    !,
    (   Term == end_of_file
    ->  Accounts = []
    ;   Term = account(_, _, _)
    ->  Accounts = [Term | Rest],
        read_accounts_loop(In, Rest)
    ;   read_accounts_loop(In, Accounts)
    ).
read_accounts_loop(_, []).

%!  save_accounts(+Accounts)
%
%   Persist the account list atomically by writing to a temporary file in the
%   same directory and renaming it over the target.
save_accounts(Accounts) :-
    accounts_file(File),
    file_directory_name(File, Dir),
    (   exists_directory(Dir)
    ->  true
    ;   make_directory(Dir)
    ),
    atom_concat(File, '.tmp', Tmp),
    setup_call_cleanup(open(Tmp, write, Out),
                       write_account_terms(Out, Accounts),
                       close(Out)),
    rename_file(Tmp, File).

write_account_terms(_, []).
write_account_terms(Out, [account(Name, Hash, Created) | Rest]) :-
    write_term(Out, account(Name, Hash, Created),
               [quoted(true), fullstop(true), nl(true)]),
    write_account_terms(Out, Rest).

%!  create_account(+Name, +Password)
%
%   Validate the username and password, ensure no duplicate exists
%   case-insensitively, hash the password with PBKDF2-SHA512, and persist.
create_account(Name, Password) :-
    valid_username(Name),
    valid_password(Password),
    with_mutex(accounts_mutex, create_account_locked(Name, Password)).

create_account_locked(Name, Password) :-
    load_accounts(Accounts),
    downcase_atom(Name, DownName),
    \+ ( member(account(Existing, _, _), Accounts),
         downcase_atom(Existing, DownExisting),
         DownName == DownExisting ),
    get_time(Time),
    Created is round(Time),
    crypto_password_hash(Password, Hash, []),
    save_accounts([account(Name, Hash, Created) | Accounts]).

%!  verify_credentials(+Name, +Password)
%
%   Semidet: succeed only when the account exists and the password matches.
%   SWI-Prolog's crypto_password_hash/2 is the verification form; there is no
%   crypto_password_verify/2 in this version.
verify_credentials(Name, Password) :-
    load_accounts(Accounts),
    member(account(Name, StoredHash, _), Accounts),
    crypto_password_hash(Password, StoredHash).

%!  set_password(+Name, +Password)
%
%   Replace the stored hash for an existing account. Fails if the account does
%   not exist.
set_password(Name, Password) :-
    valid_username(Name),
    valid_password(Password),
    with_mutex(accounts_mutex, set_password_locked(Name, Password)).

set_password_locked(Name, Password) :-
    load_accounts(Accounts),
    select(account(Name, _, Created), Accounts, Rest),
    !,
    crypto_password_hash(Password, Hash, []),
    save_accounts([account(Name, Hash, Created) | Rest]).

%!  account_names(-Names)
%
%   Sorted list of account names in the database.
account_names(Names) :-
    load_accounts(Accounts),
    findall(Name, member(account(Name, _, _), Accounts), Names0),
    sort(Names0, Names).

%!  valid_username(+Name)
%
%   Usernames must be 3..32 characters, contain only lowercase letters,
%   digits, '_', '.', '-', and start with a letter or digit.
valid_username(Name) :-
    atom(Name),
    atom_length(Name, Len),
    between(3, 32, Len),
    re_match('^[a-z0-9][a-z0-9_.-]*$', Name).

%!  valid_password(+Password)
%
%   Passwords must be at least 8 and at most 1024 characters long.
valid_password(Password) :-
    password_length(Password, Len),
    between(8, 1024, Len).

password_length(Password, Len) :- atom(Password), atom_length(Password, Len), !.
password_length(Password, Len) :- string(Password), string_length(Password, Len), !.
password_length(Password, Len) :- is_list(Password), length(Password, Len).
