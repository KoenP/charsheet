#!/usr/bin/env swipl
:- initialization(main, main).

% Resolve server/accounts.pl from this file's own location so the tool does not
% depend on (and does not warn about) the current working directory. The account
% database itself is still addressed relative to the working directory, so run
% this from the application root ('/app' in the container).
:- (   prolog_load_context(directory, Dir)
    ->  atom_concat(Dir, '/../server/accounts.pl', AccountsPath)
    ;   AccountsPath = 'server/accounts.pl'
    ),
    use_module(AccountsPath).

main(Argv) :-
    (   exists_file('server/accounts.pl')
    ->  true
    ;   format(user_error,
               'Error: run this from the application root, e.g. /app in the container.~n',
               []),
        halt(1)
    ),
    run(Argv).

run([]) :- usage, halt(0).
run(['--help']) :- usage, halt(0).
run(['--list']) :- list_accounts, halt(0).
run(['--create', User | Opts]) :- create_cli(User, Opts).
run(['--reset', User | Opts]) :- reset_cli(User, Opts).
run(_) :-
    format(user_error, 'Error: unknown command or arguments.~n', []),
    usage,
    halt(1).

usage :-
    format('Usage: reset-password.pl [--list | --create <username> [--password <password>] | --reset <username> [--password <password>]]~n', []).

list_accounts :-
    load_accounts(Accounts),
    (   Accounts = []
    ->  format('No accounts.~n', [])
    ;   forall(member(account(Name, Hash, Created), Accounts),
               (   (   ground(Hash)
                   ->  HasHash = yes
                   ;   HasHash = no
                   ),
                   format('~w\t~w\thash=~w~n', [Name, Created, HasHash])
               ))
    ).

create_cli(User, Opts) :-
    extract_password(Opts, Password, [], Source),
    !,
    (   valid_username(User)
    ->  true
    ;   format(user_error, 'Error: invalid username.~n', []),
        halt(1)
    ),
    (   create_account(User, Password)
    ->  (   Source = generated
        ->  format('Generated password: ~w~n', [Password])
        ;   true
        ),
        format('Created account ~w.~n', [User]),
        halt(0)
    ;   format(user_error,
               'Error: could not create account (duplicate name or invalid password).~n',
               []),
        halt(1)
    ).
create_cli(_, _) :-
    format(user_error, 'Error: invalid arguments for --create.~n', []),
    usage,
    halt(1).

reset_cli(User, Opts) :-
    extract_password(Opts, Password, [], Source),
    !,
    (   set_password(User, Password)
    ->  (   Source = generated
        ->  format('Generated password: ~w~n', [Password])
        ;   true
        ),
        format('Password reset for account ~w.~n', [User]),
        halt(0)
    ;   format(user_error, 'Error: account ~w not found.~n', [User]),
        halt(1)
    ).
reset_cli(_, _) :-
    format(user_error, 'Error: invalid arguments for --reset.~n', []),
    usage,
    halt(1).

extract_password(['--password', Password | Rest], Password, Rest, provided) :- !.
extract_password([], Password, [], generated) :-
    crypto_n_random_bytes(12, Bytes),
    hex_bytes(Bytes, Password).
extract_password(_, _, _, _) :- fail.

hex_bytes(Bytes, Hex) :-
    findall(Atom,
            (member(B, Bytes), format(atom(Atom), '~|~`0t~16r~2+', [B])),
            Atoms),
    atomic_list_concat(Atoms, Hex).
