#!/bin/bash

parser_tests=$(find tests/parsing -name *\.rapid)
success="TEST_SUCCESS"
had_failures="0"
tmp_file=".tmp_err_output"  # stderr of parser stored here

# Verbose output from YACC
export OCAMLRUNPARAM='p'

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

    # stdout of parser
    output=$(./parser < "$file" 2> "$tmp_file")
    # boolean result
    outcome=$(echo "$output" | grep $success)
    # true if test expected to fail
    fail_test=$(echo "$file" |  grep "fail_")


    # should fail & failed OR should pass & passed
    # not XOR'd
    if [[ $fail_test && ! $outcome ]] ||  [[ ! $fail_test && $outcome ]]
    then
        echo "success: $test_name"
    else
        echo "FAIL:    $test_name"
        printf "    " && cat "$tmp_file"
        had_failures="1"
    fi
done

rm -f "$tmp_file"
exit $had_failures
