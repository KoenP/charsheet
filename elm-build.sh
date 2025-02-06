#!/bin/bash

(cd elm && elm make src/Main.elm --output ../static/js/charsheet.js)
