#!/bin/sh

test=$1
echo "$test"

../rapid < "$test" > main.go
go build -o executable


