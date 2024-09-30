:- [inference/main].
:- [pages/select_character].
:- [pages/character_editor].

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
:- use_module(library(sgml)).
:- use_module(library(settings)).
:- use_module('inference/char_db').

user:file_search_path(html, 'client/dist').
user:file_search_path(css, 'client/dist/css').
user:file_search_path(js, 'client/dist/js').

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Handlers.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Static files.
file_search_path(static, static).
:- http_handler(root(static), serve_files_in_directory(static), [prefix]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Server-generated pages.
:- http_handler(root(.), serve_page(select_character_page), [method(get)]).
:- http_handler(root(character/CharId), serve_page(character_editor_page(CharId)),
                [method(get), id(load_character_page)]).

serve_page(Html, _Request) :-
    phrase(Html, Tokenized),
    format('Content-type: text/html~n~n'),
    print_html(Tokenized).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Character management.
:- http_handler(root(api / list_characters), h_list_characters, [method(get)]).
:- http_handler(root(api / new_character), h_new_character, [method(post)]).
:- http_handler(root(api / create_character), h_create_character, [method(post)]).

% Single-character queries.
:- http_handler(root(api / character / CharId / sheet),
                handle_with_char_snapshot(h_get_sheet, CharId),
                [method(get)]).
:- http_handler(root(api / character / CharId / options),
                handle_with_char_snapshot(h_get_options, CharId),
                [method(get)]).
:- http_handler(root(api / character / CharId / edit_character_page),
                handle_with_char_snapshot(h_get_edit_character_page, CharId),
                [method(get)]).
:- http_handler(root(api / character / CharId / equipment),
                h_get_equipment(CharId),
                [method(get)]).

% Single-character updates.
:- http_handler(root(api / character / CharId / choice), h_post_choice(CharId), [method(post)]).
:- http_handler(root(api / character / CharId / retract_choice), h_post_retract_choice(CharId), [method(post)]).
:- http_handler(root(api / character / CharId / gain_level), h_post_gain_level(CharId), [method(post)]).
:- http_handler(root(api / character / CharId / retract_gain_level),
                h_post_retract_gain_level(CharId),
                [method(post)]).
:- http_handler(root(api / character / CharId / set_base_abilities),
                h_post_set_base_abilities(CharId),
                [method(post)]).
:- http_handler(root(api / character / CharId / equip_item),
                h_post_equip_item(CharId),
                [method(post)]).
:- http_handler(root(api / character / CharId / unequip_item),
                h_post_unequip_item(CharId),
                [method(post)]).


h_list_characters(_Request) :-
    char_db:list_characters(Chars),
    reply_json_dict(Chars).
    
h_new_character(Request) :-
    http_parameters(Request, [name(Name,[])]),
    char_db:create_character(Name, Uuid),
    http_redirect(see_other, location_by_id(load_character_page(Uuid)), Request).

h_create_character(Request) :-
    http_parameters(Request, [name(Name,[])]),
    char_db:create_character(Name, Uuid),
    reply_json_dict(Uuid).
         
         
h_get_sheet(_Request) :-
    sheet_json_dict(Dict),
    reply_json_dict(Dict).

h_get_options(_Request) :-
    all_options_by_level_json(Json),
    reply_json_dict(Json).

h_get_edit_character_page(_Request) :-
    all_options_by_level_json(OptsJson),
    traits_and_bonuses_json(TBJson),
    ability_table_json_dict(AbiJson),
    reply_json_dict(_{options: OptsJson, ability_table: AbiJson, traits_and_bonuses: TBJson}).

h_get_equipment(CharId, _Request) :-
    char_db:show_inventory(CharId, Items),
    reply_json_dict(Items).

h_post_choice(CharId, Request) :-
    http_parameters(Request, [ source(SourceAtom,[]),
                               id(IdAtom,[]),
                               choice(ChoiceAtom,[])
                             ]),
    read_term_from_atom(SourceAtom, Source, []),
    read_term_from_atom(IdAtom, Id, []),
    read_term_from_atom(ChoiceAtom, Choice, []),
    char_db:withdraw_choice(CharId, Source, Id, _),
    resolve_ineligible_choices(CharId),
    char_db:record_choice(CharId, Source, Id, Choice),
    reply_json_dict("Success!").

h_post_retract_choice(CharId, Request) :-
    http_parameters(Request, [ source(SourceAtom, []),
                               id(IdAtom, [])
                             ]),
    read_term_from_atom(SourceAtom, Source, []),
    read_term_from_atom(IdAtom, Id, []),
    char_db:withdraw_choice(CharId, Source, Id, _),
    resolve_ineligible_choices(CharId),
    reply_json_dict("Success!").

h_post_gain_level(CharId, Request) :-
    http_parameters(Request, [class(Class,[])]),
    char_db:current_level(CharId, CurLevel),
    NewLevel is CurLevel + 1,
    char_db:record_gain_level(CharId, NewLevel, Class, hp_avg),
    reply_json_dict("Success!").

h_post_retract_gain_level(CharId, Request) :-
    http_parameters(Request, [level(RetractedLevel, [integer])]),

    % Withdraw all levels starting from the retracted level.
    forall((char_db:gain_level(CharId, L, _, _), L >= RetractedLevel),
           char_db:withdraw_gain_level(CharId, L, _, _)),

    % Use a character snapshot to determine which choices are now no longer valid.
    resolve_ineligible_choices(CharId),

    reply_json_dict("Success!").

h_post_set_base_abilities(CharId, Request) :-
    member(search(Params), Request),
    forall((member(Abi=ScoreStr,Params),
            read_term_from_atom(ScoreStr,Score,[]),
            integer(Score)),
           char_db:record_base_ability(CharId, Abi, Score)),
    reply_json_dict("success!").

h_post_equip_item(CharId, Request) :-
    http_parameters(Request, [item(ItemAtom,[])]),
    read_term_from_atom(ItemAtom, Item, []),
    (  equip_item_error(CharId, Item, Error)
    -> reply_json_dict(Error)
    ;  char_db:record_has(CharId, Item),
       char_db:show_inventory(CharId, Items),
       reply_json_dict(Items)
    ).

equip_item_error(CharId, Item, "You already have that item equipped.") :-
    has(CharId, Item).
equip_item_error(_, Item, "That item does not exist") :-
    \+ item_exists(Item).

h_post_unequip_item(CharId, Request) :-
    http_parameters(Request, [item(ItemAtom,[])]),
    read_term_from_atom(ItemAtom, Item, []),
    char_db:withdraw_has(CharId, Item),
    char_db:show_inventory(CharId, Items),
    reply_json_dict(Items).

handle_with_char_snapshot(Handler, CharId, Request) :-
    snapshot(
        (load_character_from_db(CharId),
         call(Handler, Request),
         abolish_private_tables)).

resolve_ineligible_choices(CharId) :-
    abolish_private_tables,
    snapshot((load_character_from_db(CharId),
              findall(Origin-Id, problem(not_eligible(Origin, Id, _)), ChoicesToUndo),
              abolish_private_tables)),
    forall(member(Origin-Id, ChoicesToUndo),
           char_db:withdraw_choice(CharId, Origin, Id, _)),
    (ChoicesToUndo \= [] -> resolve_ineligible_choices(CharId) ; true).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Persistency.
load_character_from_db(CharId) :-
    ground(CharId),
    forall(char_db:name(CharId, Name), assert(name(Name))),
    forall(char_db:base_ability(CharId, Ability, Score), assert(base_ability(Ability, Score))),
    forall(char_db:gain_level(CharId, Level, Class, HpMode), assert(gain_level(Level, Class, HpMode))),
    forall(char_db:choice(CharId, Origin, Id, Choice), assert(choice(Origin, Id, Choice))),
    forall(char_db:has(CharId, Item), assert(has(Item))),
    !.

load_by_name(Name) :-
    char_db:name(CharId, Name),
    load_character_from_db(CharId),
    !.

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
