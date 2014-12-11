package main

type Int *int
type Float *float64
type Bool *bool
type String *string

func IntToBool(i Int) bool {
	return i == nil || *i == 0
}

func FloatToBool(f Float) bool {
	return f == nil || *f == 0.0
}

func BoolToBool(b Bool) bool {
	return b == nil || *b == false
}

func StringToBool(s String) bool {
	return s == nil || *s == ""
}
