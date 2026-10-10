login_page --> login_page([]).

login_page(Errors) -->
    { findall(div(class=error, E), member(E, Errors), ErrorHtml) },
    page([title('Character sheet generator: login'), \theme_head_extras],
         body([div(class='page-header',
                   [h1('Log in'),
                    p('Welcome back to your character sheet.')
                   ]),
               div(class=panel,
                   [\html(ErrorHtml),
                    form([action='/api/login', method=post],
                         [div(class='form-group',
                              [label([for=username], 'User name'),
                               input([type=text, id=username, name=username, placeholder='User name', required=true])]),
                          div(class='form-group',
                              [label([for=password], 'Password'),
                               input([type=password, id=password, name=password, placeholder='Password', required=true])]),
                          div(class='form-actions',
                              input([type=submit, class='btn', value='Log in']))]),
                    p(class='form-footer', a(href='/register', 'Create an account'))
                   ]),
               \theme_toggle
              ])).
