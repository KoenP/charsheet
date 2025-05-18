#!/bin/bash

cabal build
cp ./dist-newstyle/build/javascript-ghcjs/ghc-9.10.0.20240413/charsheet-frontend-0.1.0.0/x/charsheet-frontend/build/charsheet-frontend/charsheet-frontend.jsexe/all.js ../static/js/charsheet.js
