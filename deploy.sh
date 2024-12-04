#!/bin/bash

clear
(cd elm && elm make src/Main.elm --output ../static/js/charsheet.js)
scp -i ~/.ssh/id_rsa static/js/charsheet.js koen@192.168.68.61:/home/koen/charsheet/static/js

