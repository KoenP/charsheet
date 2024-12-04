#!/bin/bash

while read NAME
do
    curl "https://www.dnd5eapi.co/api/spells/${NAME}" | jq >> api_spells.json
done < api_indices
