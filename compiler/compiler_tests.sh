#!/bin/bash

stdlib_loc="lib/"
compiler_tests=$(find tests/compiler -name *\.rapid)
had_failures="0"
tmp_file="$(pwd)/.test_output"  # stderr of parser stored here
go_file="${stdlib_loc}main.go"
suffix=".output"
executable="binary"

reduce_path_to_test_name () {
    local fullpath=$1
    # strip the file type & set a global variable
    testpath="${fullpath%.*}"
    test_name="${testpath##*/}"     # strip the preceding path

}

for file in $compiler_tests
do
    reduce_path_to_test_name "$file"

    # true if test expected to fail
    fail_test=$(echo "$file" |  grep "fail_")
    # write generated code to file
    ./rapid < "$file" > "$go_file"

    # go to build dir w/ std lib
    pushd "$stdlib_loc" &> /dev/null
    go build -o "$executable"
    outcome=$("./$executable" &> "$tmp_file")
    rm -f "$executable"
    popd &> /dev/null

    # true if test expected to fail
    fail_test=$(echo "$file" |  grep "fail_")

    if [[ $fail_test && !$outcome  ]]
    then
        echo "success: $test_name"
    elif [[ outcome ]] && [[ ! $(diff "$tmp_file" "$testpath$suffix") ]]
    then
        echo "success: $test_name"
    else
        echo "FAIL:    $test_name"
        had_failures="1"

        printf "Expected: {\n"
        cat "$testpath$suffix"
        printf "}\n"
        echo

        printf "Recieved: {\n"
        cat "$tmp_file"
        printf "}\n"
        echo

        printf "Generated Code: [[[\n"
        gofmt "$go_file"
        printf "]]]\n"
        echo
    fi
done

rm -f "$tmp_file"
rm -f "$go_file"
exit $had_failures

