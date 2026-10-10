:- use_module(library(http/html_write)).

theme_head_extras -->
    html([link([rel=stylesheet, href='/static/css/rosepine.css'], []),
          script([type='text/javascript'],
                 ['try { var t = localStorage.getItem(\'charsheet-theme\'); if (t === \'rose-pine\') document.documentElement.setAttribute(\'data-theme\', \'rose-pine\'); } catch (e) {}'])]).

theme_toggle -->
    html([button([id='theme-toggle',
                  class='theme-toggle',
                  type='button',
                  title='Switch theme',
                  'aria-label'='Switch theme',
                  'aria-pressed'='false'],
                 ['🌙']),
          script([type='text/javascript', src='/static/js/theme.js'], [])]).
