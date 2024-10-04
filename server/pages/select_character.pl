select_character_page -->
    { http_session_data(logged_in_as(UserName)),
      format(string(Welcome), 'Welcome ~w!', UserName)
    },
    page(title('Character sheet generator'),
         body([ h2(Welcome),
                form([action='/api/logout', method=post], [input([type=submit, value='Log out'])]),
                h3('Create a new character...'),
                \new_character_box_html,
                h3('... or select an existing one'),
                \character_list_html
              ])).

new_character_box_html -->
    html(form([action='/api/new_character', method=post],
              [input([type=text, name=name, placeholder='New character name', required=true]),
               input([type=submit, value='Create'])
              ]
             )).

    %html([ input([type=text, placeholder='New character name']),
    %       button(['Create'])
    %     ]).

character_list_html -->
    %{char_db:list_characters(Chars)},
    {list_characters(Chars)},
    html([ul(\character_list_items_html(Chars))]).

character_list_items_html([]) --> [].
character_list_items_html([Name|Names]) -->
    html([ li(a(href=location_by_id(load_character_page(Name)), Name)),
           \character_list_items_html(Names)]).
    
