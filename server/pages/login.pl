login_page --> login_page([]).

login_page(Errors) -->
    { findall(p(class=error, E), member(E, Errors), ErrorHtml) },
    page(title('Character sheet generator: login'),
         body([h3('Log in'),
               form([action='/api/login', method=post],
                    [input([type=text, name=username, placeholder='User name', required=true]),
                     input([type=password, name=password, placeholder='Password', required=true]),
                     input([type=submit, value='Log in'])]),
               p(a(href='/register', 'Create an account'))
               | ErrorHtml])).
