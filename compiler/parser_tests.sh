#!/bin/bash

parser_tests=$(find tests/parsing -name *\.rapid)
success="TEST_SUCCESS"
had_failures="0"
tmp_file=".tmp_err_output"  # stderr of parser stored here

# Verbose output from YACC
export OCAMLRUNPARAM='p'

reduce_path_to_test_name () {
    local fullpath=$1
    local filename="${fullpath##*/}" # strip the preceding path
    test_name="${filename%.*}"       # strip the file type & set a global variable
}


for file in $parser_tests
do
    reduce_path_to_test_name "$file"

    # stdout of parser
    output=$(./parser < "$file" 2> "$tmp_file")
    # boolean result
    outcome=$(echo "$output" | grep $success)
    # true if test expected to fail
    fail_test=$(echo "$file" |  grep "fail_")


    # should fail & failed OR should pass & passed
    #  ! XOR'd
    if [[ $fail_test && ! $outcome ]] ||  [[ ! $fail_test && $outcome ]]
    then
        echo "success: $test_name"
    else
        echo
        echo "FAIL:    $test_name"
        printf "    " && cat "$tmp_file"
        echo
        had_failures="1"
    fi
done

rm -f "$tmp_file"
exit $had_failures
