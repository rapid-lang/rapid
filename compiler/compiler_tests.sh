#!/bin/bash

compiler_tests=$(find tests/compiler -name *\.rapid)
had_failures="0"
tmp_file=".test_output"  # stderr of parser stored here
suffix=".output"

reduce_path_to_test_name () {
    local fullpath=$1
    # strip the file type & set a global variable
    testpath="${fullpath%.*}"
    test_name="${testpath##*/}"     # strip the preceding path

}

for file in $compiler_tests
do
    reduce_path_to_test_name "$file"

    ./rapid < "$file" > "main.go"
    # boolean result
    outcome=$(go run main.go > "$tmp_file")

    if [[ outcome ]] && [[ ! $(diff "$tmp_file" "$testpath$suffix") ]]
    then
        echo "success: $test_name"
    else
        echo "FAIL:    $test_name"
        had_failures="1"

        printf "Expected: {\n"
        cat "$testpath$suffix"
        printf "}\n"

        printf "Generated Code: [[[\n"
        cat "main.go"
        printf "]]]\n"
    fi
done

rm -f "$tmp_file"
exit $had_failures

