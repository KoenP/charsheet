:- use_module(library(http/html_write)).

register_page --> register_page([]).

register_page(Errors) -->
    { findall(p(class=error, E), member(E, Errors), ErrorHtml) },
    page(title('Character sheet generator: register'),
         body([h3('Create an account'),
               form([action='/api/register', method=post],
                    [input([type=text, name=username, placeholder='User name', required=true]),
                     input([type=password, name=password, placeholder='Password', required=true]),
                     input([type=password, name=password_confirm, placeholder='Confirm password', required=true]),
                     \captcha_widget_markup,
                     input([type=submit, value='Create account'])]),
               p(a(href='/', 'Log in instead'))
               | ErrorHtml])).

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
