package main

type Int *int
type Float *float64
type Bool *bool
type String *string

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
