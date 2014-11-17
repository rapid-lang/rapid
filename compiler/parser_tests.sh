#!/bin/bash

parser_tests=$(find tests/parsing -name *\.rapid)

success="TEST_SUCCESS"
all_successful="0"

for file in $parser_tests
do
    if ./parser < "$file" 2> /dev/null | grep -q $success
    then
        echo "$file worked"
    else
        echo "$file did not work"
        all_successful="1"
    fi
done

exit $all_successful
