#!/bin/bash

parser_tests=$(find tests/parsing -name *\.rapid)

success="TEST_SUCCESS"
all_successful="0"

reduce_path_to_test_name () {
    local fullpath=$1
    # strip the preceding path
    local filename="${fullpath##*/}"
    # strip the file type & set a global variable
    test_name="${filename%.*}"
}

for file in $parser_tests
do
    reduce_path_to_test_name "$file"

    # switch on if a file should fail or not
    if echo "$file" |  grep -q "fail_"
    then

        # test should fail
        if ./parser < "$file" 2> /dev/null | grep -q $success
        then
            echo "FAIL:    $test_name"
            all_successful="1"
        else
            echo "success: $test_name"
        fi

    else

        # test should not fail
        if ./parser < "$file" 2> /dev/null | grep -q $success
        then
            echo "success: $test_name"
        else
            echo "FAIL:    $test_name"
            all_successful="1"
        fi
    fi
done

exit $all_successful
