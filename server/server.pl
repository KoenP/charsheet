:- [inference/main].
:- [storage].
:- [pages/login].
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
:- use_module(library(http/http_session)).
:- use_module(library(sgml)).
:- use_module(library(settings)).

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
:- http_handler(root(.), serve_page(login_page), [method(get), id(login_page)]).
:- http_handler(root(select), serve_page_if_logged_in(select_character_page),
                [method(get), id(select_character_page)]).
:- http_handler(root(character/CharId), serve_page_if_logged_in(character_editor_page(CharId)),
                [method(get), id(load_character_page)]).

serve_page(Html, _Request) :-
    phrase(Html, Tokenized),
    format('Content-type: text/html~n~n'),
    print_html(Tokenized).
serve_page_if_logged_in(Html, Request) :-
    (  http_session_data(logged_in_as(_))
    -> serve_page(Html, Request)
    ;  http_redirect(see_other, location_by_id(login_page), Request)
    ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% User account management.
:- http_handler(root(api / login), h_login, [method(post)]).

h_login(Request) :-
    http_parameters(Request, [username(Name,[])]),
    http_session_assert(logged_in_as(Name)),
    http_redirect(see_other, location_by_id(select_character_page), Request).

:- http_handler(root(api / logout), h_logout, [method(post)]).

h_logout(Request) :-
    http_current_session(Id, _),
    http_close_session(Id),
    http_redirect(see_other, location_by_id(login_page), Request).

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
    list_characters(Chars),
    reply_json_dict(Chars).
    
h_new_character(Request) :-
    http_parameters(Request, [name(Name,[])]),
    with_new_character(Name, true),
    http_redirect(see_other, location_by_id(load_character_page(Name)), Request).

h_create_character(Request) :-
    http_parameters(Request, [name(Name,[])]),
    with_new_character(Name, true),
    reply_json_dict(Name).
         
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
    with_loaded_character(
        CharId,
        (findall(I, (has(Tm), term_string(Tm, I)), Items),
         reply_json_dict(Items))).

h_post_choice(CharId, Request) :-
    http_parameters(Request, [ source(SourceAtom,[]),
                               id(IdAtom,[]),
                               choice(ChoiceAtom,[])
                             ]),
    read_term_from_atom(SourceAtom, Source, []),
    read_term_from_atom(IdAtom, Id, []),
    read_term_from_atom(ChoiceAtom, Choice, []),
    withdraw_character_fact(CharId, choice(Source, Id, _)),
    record_character_fact(CharId, choice(Source, Id, Choice)),
    reply_json_dict("Success!").

h_post_retract_choice(CharId, Request) :-
    http_parameters(Request, [ source(SourceAtom, []),
                               id(IdAtom, [])
                             ]),
    read_term_from_atom(SourceAtom, Source, []),
    read_term_from_atom(IdAtom, Id, []),
    withdraw_character_fact(CharId, choice(Source, Id, _)),
    reply_json_dict("Success!").

h_post_gain_level(CharId, Request) :-
    http_parameters(Request, [class(Class,[])]),
    with_loaded_character(
        CharId,
        ( level(CurLevel),
          NewLevel is CurLevel + 1,
          record_character_fact(CharId, gain_level(NewLevel, Class, hp_avg))) ),
    reply_json_dict("Success!").

h_post_retract_gain_level(CharId, Request) :-
    http_parameters(Request, [level(RetractedLevel, [integer])]),
    withdraw_gain_level(CharId, RetractedLevel),
    resolve_ineligible_choices(CharId),
    reply_json_dict("Success!").

h_post_set_base_abilities(CharId, Request) :-
    member(search(Params), Request),
    findall(Abi=Score,
            (member(Abi=ScoreStr, Params), read_term_from_atom(ScoreStr, Score, []), integer(Score)),
            AbiScores),
    set_base_abilities(CharId, AbiScores),
    reply_json_dict("success!").

h_post_equip_item(CharId, Request) :-
    http_parameters(Request, [item(ItemAtom,[])]),
    read_term_from_atom(ItemAtom, Item, []),
    with_loaded_character(
        CharId,
        (  equip_item_error(Item, Error)
        -> reply_json_dict(Error)
        ;  record_character_fact(CharId, has(Item)),
           assert(has(Item)),
           findall(I, (has(Tm), term_string(Tm, I)), Items),
           reply_json_dict(Items))).

equip_item_error(Item, "You already have that item equipped.") :-
    has(Item).
equip_item_error(Item, "That item does not exist") :-
    \+ item_exists(Item).

% TODO
h_post_unequip_item(CharId, Request) :-
    http_parameters(Request, [item(ItemAtom,[])]),
    read_term_from_atom(ItemAtom, Item, []),
    with_loaded_character(
        CharId,
        ( retractall(has(Item)),
          character_file_path(CharId, Path),
          rewrite_character_file(Path),
          findall(I, (has(Tm), term_string(Tm, I)), Items),
          reply_json_dict(Items))).

handle_with_char_snapshot(Handler, CharId, Request) :-
    with_loaded_character(CharId, (call(Handler, Request), abolish_private_tables)).