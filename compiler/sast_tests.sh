#!/bin/bash

success="TEST_SUCCESS"
had_failures="0"
tmp_file=".tmp_err_output"  # stderr of parser stored here

reduce_path_to_test_name () {
    local fullpath=$1
    local filename="${fullpath##*/}" # strip the preceding path
    test_name="${filename%.*}"       # strip the file type & set a global variable
}

run_test() {
    local should_fail=$1

    # stdout of parser
    output=$(./sast < "$file" 2> "$tmp_file")
    # boolean result
    outcome=$(echo "$output" | grep $success)
    # true if test expected to fail


    # should fail & failed OR should pass & passed
    #  ! XOR'd
    if [[ $should_fail && ! $outcome ]] ||  [[ ! $should_fail && $outcome ]]
    then
        echo "success: $test_name"
    else
        echo "FAIL:    $test_name"
        echo "$output"
        cat "$tmp_file"
        had_failures="1"
    fi
    rm -f "$tmp_file"
}

sast_tests=$(find tests/sast -name *\.rapid)
echo "SEMANTIC CHECKING SAST TESTS"
for file in $sast_tests
do
    reduce_path_to_test_name "$file"
    fail_test=$(echo "$file" |  grep "fail_")
    run_test "$fail_test"
done

compiler_tests=$(find tests/compiler -name *\.rapid)
echo "SEMANTIC CHECKING COMPILER TESTS"
for file in $compiler_tests
do
    reduce_path_to_test_name "$file"
    fail_test=""
    run_test "$fail_test"
done

rm -f "$tmp_file"
exit $had_failures

