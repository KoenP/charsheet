#!/bin/bash

clear
(cd elm && elm make src/Main.elm --output ../static/js/charsheet.js) && swipl server.pl
