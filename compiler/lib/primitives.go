package main

import "strconv"
import "fmt"
type Int *int
type Float *float64
type Bool *bool
type String *string

var println = fmt.Println
var printf = fmt.Printf
/*
 * BOOLEANS
 */
func IntToBool(i Int) Bool {
	tmp := !(i == nil || *i == 0)
	return Bool(&tmp)
}

func FloatToBool(f Float) Bool {
	tmp := !(f == nil || *f == 0.0)
	return Bool(&tmp)
}

func BoolToBool(b Bool) Bool {
	tmp := !(b == nil || *b == false)
	return Bool(&tmp)
}

func StringToBool(s String) Bool {
	tmp := !(s == nil || *s == "")
	return Bool(&tmp)
}

/*
 * FLOATS
 */
func IntToFloat(i Int) Float {
	tmp := float64(*i)
	return Float(&tmp)
}

func FloatToFloat(f Float) Float {
	tmp := float64(*f)
	return Float(&tmp)
}

/*
 * INTS
 */
func FloatToInt(f Float) Int {
	tmp := int(*f)
	return Int(&tmp)
}

func IntToInt(i Int) Int {
	tmp := int(*i)
	return Int(&tmp)
}

/*
 * Strings
 */
func IntToString(i Int) String {
	tmp := strconv.Itoa(*i)
	return String(&tmp)
}

func FloatToString(f Float) String {
	tmp := strconv.FormatFloat(*f, 'f', -1, 64)
	return String(&tmp)
}

func BoolToString(b Bool) String {
	tmp := strconv.FormatBool(*b)
	return String(&tmp)
}

func StringToString(s String) String {
	tmp := *s
	return String(&tmp)
}
