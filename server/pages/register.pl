:- use_module(library(http/html_write)).

register_page --> register_page([]).

register_page(Errors) -->
    { findall(div(class=error, E), member(E, Errors), ErrorHtml) },
    page([title('Character sheet generator: register'), \theme_head_extras],
         body([div(class='page-header',
                   [h1('Create an account'),
                    p('Join to start building character sheets.')
                   ]),
               div(class=panel,
                   [\html(ErrorHtml),
                    form([action='/api/register', method=post],
                         [div(class='form-group',
                              [label([for=username], 'User name'),
                               input([type=text, id=username, name=username, placeholder='User name', required=true])]),
                          div(class='form-group',
                              [label([for=password], 'Password'),
                               input([type=password, id=password, name=password, placeholder='Password', required=true])]),
                          div(class='form-group',
                              [label([for=password_confirm], 'Confirm password'),
                               input([type=password, id=password_confirm, name=password_confirm, placeholder='Confirm password', required=true])]),
                          \captcha_widget_markup,
                          div(class='form-actions',
                              input([type=submit, class='btn', value='Create account']))]),
                    p(class='form-footer', a(href='/', 'Log in instead'))
                   ]),
               \theme_toggle
              ])).

captcha_widget_markup --> { \+ captcha_required }, !, [].
captcha_widget_markup -->
    { captcha_required,
      captcha_widget_link(Link),
      !,
      Label = label([data-mcaptcha_url=Link,
                     for='mcaptcha__token',
                     id='mcaptcha__token-label'],
                    ['mCaptcha authorization token. ',
                     input([type=text, name='mcaptcha__token', id='mcaptcha__token'])])
    },
    html([Label,
          div([id='mcaptcha__widget-container'], []),
          script([src='/static/js/mcaptcha-glue.js'], [])]).
captcha_widget_markup -->
    { captcha_required, \+ captcha_widget_link(_) },
    throw(error(instantiation_error, captcha_widget_link_failed)).
