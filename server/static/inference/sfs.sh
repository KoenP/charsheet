#!/bin/bash

cat inference/directives.pl
echo 'listing.' | swipl --quiet inference/main.pl 
