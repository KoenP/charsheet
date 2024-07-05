#!/bin/bash

echo 'main.' | swipl --quiet buildutil/write_predefined_preds_and_ops.pl > build/predefined_predicates_and_ops.pl
