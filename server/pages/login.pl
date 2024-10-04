login_page -->
    page(title('Character sheet generator: login'),
         body(form([action='/api/login', method=post],
                   [input([type=text, name=username, placeholder='User name', required=true]),
                    input([type=submit, value='Log in'])
                   ]))).
