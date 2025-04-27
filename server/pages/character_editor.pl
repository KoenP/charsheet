% Most of the work here is done by the Elm app, all we need to do is set it up with the character ID.

:- use_module(library(http/js_write)).

character_editor_page(CharName) -->
    page([title(CharName),
          script([type='text/javascript', src='/static/js/charsheet.js'], []),
          link([rel=stylesheet, href='/static/css/printable-char-sheet.css'], []),
          link([rel=stylesheet, href='https://fonts.googleapis.com/css2?family=Dosis:wght@400;700&display=swap'], [])
         ],
         body([div([id=app], []),
               \js_script({|javascript(CharName)||
                            function getCharName() {
                                return CharName;
                            }
                          |})
              ]
             )).


%\js_script({|javascript(CharName)||
          %            var app = Elm.Main.init( {
          %                node: document.getElementById("app"),
          %                flags: CharName
          %            });
          %           |})
