select_character_page -->
    { http_session_data(logged_in_as(UserName)),
      format(string(Welcome), 'Welcome, ~w!', UserName)
    },
    page([title('Character sheet generator'), \theme_head_extras],
         body([div(class='page-header',
                   [h1(Welcome),
                    form([class='logout-form', action='/api/logout', method=post],
                         [input([type=submit, class='btn btn-secondary', value='Log out'])])
                   ]),
               div(class=panel,
                   [h2('Create a new character'),
                    \new_character_box_html
                   ]),
               div(class='character-list-panel',
                   [h2('... or select an existing one'),
                    \character_list_html
                   ]),
               \theme_toggle
              ])).

new_character_box_html -->
    html(form([action='/api/new_character', method=post],
              [div(class='form-group',
                   [input([type=text, name=name, placeholder='New character name', required=true])]),
               div(class='form-actions',
                   input([type=submit, class='btn', value='Create']))
              ]
             )).

character_list_html -->
    {list_characters(Chars)},
    html(ul(class='character-list', \character_list_items_html(Chars))).

character_list_items_html([]) --> [].
character_list_items_html([Id-Name|Chars]) -->
    html([ li(a([class='character-card', href=location_by_id(load_character_page(Id))], Name)),
           \character_list_items_html(Chars)]).
