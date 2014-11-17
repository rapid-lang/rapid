#!/bin/bash

parser_tests=$(find tests/parsing -name *\.rapid)

success="TEST_SUCCESS"
all_successful="0"

for file in $parser_tests
do
    # switch on if a file should fail or not
    if echo "$file" |  grep -q "fail_"
    then

        # test should fail
        if ./parser < "$file" 2> /dev/null | grep -q $success
        then
            echo "FAIL:    $file"
            all_successful="1"
        else
            echo "success: $file worked"
        fi

    else

        # test should not fail
        if ./parser < "$file" 2> /dev/null | grep -q $success
        then
            echo "success: $file worked"
        else
            echo "FAIL:    test $file"
            all_successful="1"
        fi
    fi
done

exit $all_successful
