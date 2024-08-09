:- [inference/main].

:- table trait/2, known_spell/6, bonus/2, ability/2, class_level/1 as private.

:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_parameters)).
:- use_module(library(http/http_files)).
:- use_module(library(http/json)).
:- use_module(library(http/http_json)).
:- use_module(library(http/http_header)).
:- use_module(library(http/http_server_files)).
:- use_module(library(http/http_path)).
:- use_module(library(http/http_error)).
:- use_module(library(http/http_cors)).
:- use_module(library(sgml)).
:- use_module(library(settings)).
:- use_module('inference/char_db').

user:file_search_path(html, 'client/dist').
user:file_search_path(css, 'client/dist/css').
user:file_search_path(js, 'client/dist/js').

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Handlers.

:- set_setting(http:cors, [*]).

% Static files.
:- http_handler(css(.), serve_files_in_directory(css), [prefix]).
:- http_handler(js(.), serve_files_in_directory(js), [prefix]).

% Character management.
:- http_handler(root(list_characters), h_list_characters, [method(get)]).
:- http_handler(root(new_character), h_new_character, [method(post)]).

% Single-character queries.
:- http_handler(root(character / CharId / sheet),
                handle_with_char_snapshot(h_get_sheet, CharId),
                [method(get)]).
:- http_handler(root(character / CharId / options),
                handle_with_char_snapshot(h_get_options, CharId),
                [method(get)]).
:- http_handler(root(character / CharId / edit_character_page),
                handle_with_char_snapshot(h_get_edit_character_page, CharId),
                [method(get)]).

% Single-character updates.
:- http_handler(root(character / CharId / choice), h_post_choice(CharId), [method(post)]).
:- http_handler(root(character / CharId / gain_level), h_post_gain_level(CharId), [method(post)]).
:- http_handler(root(character / CharId / set_base_abilities),
                h_post_set_base_abilities(CharId),
                [method(post)]).

h_list_characters(_Request) :-
    cors_enable,
    char_db:list_characters(Chars),
    reply_json_dict(Chars).
    
h_new_character(Request) :-
    cors_enable,
    http_parameters(Request, [name(Name,[])]),
    char_db:create_character(Name, Uuid),
    reply_json_dict(Uuid).
         
h_get_sheet(_Request) :-
    cors_enable,
    sheet_json_dict(Dict),
    reply_json_dict(Dict).

h_get_options(_Request) :-
    cors_enable,
    all_options_by_level_json(Json),
    reply_json_dict(Json).

h_get_edit_character_page(_Request) :-
    cors_enable,
    all_options_by_level_json(OptsJson),
    traits_and_bonuses_json(TBJson),
    ability_table_json_dict(AbiJson),
    reply_json_dict(_{options: OptsJson, ability_table: AbiJson, traits_and_bonuses: TBJson}).

h_post_choice(CharId, Request) :-
    cors_enable,
    http_parameters(Request, [ source(SourceAtom,[]),
                               id(IdAtom,[]),
                               choice(ChoiceAtom,[])
                             ]),
    read_term_from_atom(SourceAtom, Source, []),
    read_term_from_atom(IdAtom, Id, []),
    read_term_from_atom(ChoiceAtom, Choice, []),
    char_db:record_choice(CharId, Source, Id, Choice),
    reply_json_dict("Success!").

h_post_gain_level(CharId, Request) :-
    cors_enable,
    http_parameters(Request, [class(Class,[])]),
    char_db:current_level(CharId, CurLevel),
    NewLevel is CurLevel + 1,
    char_db:record_gain_level(CharId, NewLevel, Class, hp_avg),
    reply_json_dict("Success!").

h_post_set_base_abilities(CharId, Request) :-
    cors_enable,
    member(search(Params), Request),
    forall((member(Abi=ScoreStr,Params),
            read_term_from_atom(ScoreStr,Score,[]),
            integer(Score)),
           char_db:record_base_ability(CharId, Abi, Score)),
    reply_json_dict("success!").

handle_with_char_snapshot(Handler, CharId, Request) :-
    snapshot(
        (load_character_from_db(CharId),
         call(Handler, Request),
         abolish_private_tables)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Persistency.
load_character_from_db(CharId) :-
    ground(CharId),
    forall(char_db:name(CharId, Name), assert(name(Name))),
    forall(char_db:base_ability(CharId, Ability, Score), assert(base_ability(Ability, Score))),
    forall(char_db:gain_level(CharId, Level, Class, HpMode), assert(gain_level(Level, Class, HpMode))),
    forall(char_db:choice(CharId, Origin, Id, Choice), assert(choice(Origin, Id, Choice))).

%! run_server
%
%  Run an HTTP server on port 8000.
%
%  The server responds to the following GET requests:
%   - =|/|= : Serves =|static/index.html|=.
%   - =|/sheet|= : Serves a dynamically generated HTML rendering of the
%      current character sheet.
%   - Any other path: Serves the matching file path in the =|static|= folder.
%
%  The server responds to the following POST requests:
%   - =|/request/todo|=: Serve the current character's =|todo|= as JSON.
%   - =|/request/query?q=_|=: Run the query =|q|= which should contain a
%      variable =|X|=. The server responds with whatever =|X|= unified with,
%      rendered as plaintext.
%   - =|/request/list_characters|=: Replies with a JSON object
%      containing at least a "list" field with a list of characters
%      currently saved on the server, and optionally a "current" field
%      with the name of the currently loaded character.
%   - =|/request/new_character?name=_|=: Create a new character with the
%      given name.
%   - =|/request/load_character?name=_|=: Load the given character
%      with matching =name=.
%   - =|/request/save_character|=: Save all changes made to the
%      current character.
%   - =|/request/choice?source=_&id=_&choice=_|=: Register the given
%      choice for the character. This is immediately committed to permanent
%      storage.
:- initialization(run_server).

run_server :-
    http_server(http_dispatch, [port(8000)]).

remote_query(Request) :-
    member(path_info(PathInfo), Request),
    files_ex:strip_trailing_slash(PathInfo, Stripped),
    writeln("Access-Control-Allow-Origin: *"),
    remote_query(Request, Stripped).
remote_query(Request, '/query') :-
    http_parameters(Request, [q(QueryString,[])]),
    format('Content-type: text/plain~n~n'),
    read_term_from_atom(QueryString, Query, [variables([X])]),
    call(Query),
    write_term(X, [quoted(true)]).
remote_query(_, '/name') :-
    name(Name),
    reply_json_dict(Name).
remote_query(_, '/list_characters') :-
    findall(Char, saved_character(Char), Chars),
    (  name(Name)
    -> !, reply_json_dict(_{list: Chars, current: Name})
    ;  reply_json_dict(_{list: Chars})
    ).
remote_query(Request, '/new_character') :-
    http_parameters(Request, [name(Name,[])]),
    initialize_new_character(Name),
    reply_json_dict("success!").
remote_query(_, '/ability_table') :-
    ability_table_json_dict(Dict),
    reply_json_dict(Dict).
remote_query(Request, '/set_base_abilities') :-
    member(search(Params), Request),
    forall((member(Abi=ScoreStr,Params),
            read_term_from_atom(ScoreStr,Score,[]),
            integer(Score)),
           update_base_ability(Abi, Score)),
    reply_json_dict("success!").
remote_query(Request, '/choice') :-
    http_parameters(Request, [source(SourceStr,[]),
                              id(IdStr,[]),
                              choice(ChoiceStr,[])]),
    term_string(Source, SourceStr),
    term_string(Id, IdStr),
    term_string(Choice, ChoiceStr),
    retractall(choice(Source, Id, _)),
    assert(choice(Source, Id, Choice)),
    resolve_not_eligible,
    write_character_file,
    reply_json_dict("success!").
remote_query(_, '/sheet') :-
    sheet_json_dict(Dict),
    reply_json_dict(Dict).
remote_query(_, '/options') :-
    all_options_by_level_json(Json),
    % findall(J, options_json(_,_,J), Json),
    reply_json_dict(Json).
remote_query(_, '/edit_character_page') :-
    all_options_by_level_json(OptsJson),
    traits_and_bonuses_json(TBJson),
    ability_table_json_dict(AbiJson),
    reply_json_dict(_{options: OptsJson, ability_table: AbiJson, traits_and_bonuses: TBJson}).
remote_query(Request, '/gain_level') :-
    http_parameters(Request, [class(Class,[])]),
    level(CurLevel),
    NewLevel is CurLevel + 1,
    assert(gain_level(NewLevel, Class, hp_avg)), % TODO: option to roll for HP/manually specify
    write_character_file,
    reply_json_dict("success!").
remote_query(_, '/cur_level') :-
    level(Level),
    reply_json_dict(Level).
remote_query(_, '/list_class_options') :- % TODO: filter for available class options
    findall(O, class_option(O), Options),
    reply_json_dict(Options).
remote_query(_, '/equipment') :-
    equipment_json_dict(D),
    reply_json_dict(D).
remote_query(Request, '/unequip_weapon') :-
    http_parameters(Request, [base_weapon(BaseWeapon,[]), enchantment(EnchantmentAtom, [])]),
    atom_number(EnchantmentAtom, Enchantment),
    destructure_weapon_or_variant(Weapon, BaseWeapon, Enchantment),
    retractall(has(Weapon)),
    equipment_json_dict(D),
    reply_json_dict(D).
remote_query(Request, '/equip_weapon') :-
    http_parameters(Request, [weapon(Weapon,[])]),
    assert(has(Weapon)),
    equipment_json_dict(D),
    reply_json_dict(D).
    
quoted_term_string(T, S) :-
    term_string(T, S, [quoted(true)]).

%term_to_json(Atomic, Atomic) :- atomic(Atomic), !.
%term_to_json(List, Json) :- maplist(term_to_json, List, Json), !.
%term_to_json(Compound, _{functor: Functor, args: ArgsJson}) :-
%    Compound =.. [Functor|Args],
%    maplist(term_to_json, Args, ArgsJson).
