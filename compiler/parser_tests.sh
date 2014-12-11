#!/bin/bash

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

run_test() {
    local should_fail=$1

    # stdout of parser
    output=$(./parser < "$file" 2> "$tmp_file")
    # boolean result
    outcome=$(echo "$output" | grep $success)

    # should fail & failed OR should pass & passed
    #  ! XOR'd
    if [[ $should_fail && ! $outcome ]] ||  [[ ! $should_fail && $outcome ]]
    then
        echo "success: $test_name"
    else
        echo
        echo "FAIL:    $test_name"
        printf "    " && cat "$tmp_file"
        echo
        had_failures="1"
    fi
}


parser_tests=$(find tests/parsing -name *\.rapid)
echo "PARSING PARSING TESTS"
for file in $parser_tests
do
    reduce_path_to_test_name "$file"
    fail_test=$(echo "$file" |  grep "fail_")
    run_test "$fail_test"
done


sast_tests=$(find tests/sast -name *\.rapid)
echo "PARSING SAST TESTS"
for file in $sast_tests
do
    reduce_path_to_test_name "$file"
    fail_test=false
    run_test ""
done


compiler_tests=$(find tests/compiler -name *\.rapid)
echo "PARSING COMPILER TESTS"
for file in $compiler_tests
do
    reduce_path_to_test_name "$file"
    fail_test=false
    run_test ""
done


rm -f "$tmp_file"
exit $had_failures
