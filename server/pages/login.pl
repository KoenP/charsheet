login_page -->
    page(
        title('Character sheet generator: login'),
        body(
            [h3('Proceed without logging in'),
             form(
                 [action='/api/new_character', method=post],
                 [input([type=text, name=name, placeholder='New character name', required=true]),
                  input([type=submit, value='Create public character']),
                  p('(everyone with the link can edit a public character)')]),

             h3('or log into your account'),
             form([action='/api/login', method=post],
                  [input([type=text, name=username, placeholder='User name', required=true]),
                  input([type=submit, value='Log in'])
                  ])])).
