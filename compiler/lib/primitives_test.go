package main

import "testing"

func TestInts(t *testing.T) {
	tmp := 7
	var i Int = &tmp

	if !*IntToBool(i) {
		t.Error("Int i should not cast to false")
	}

	var b Int
	if *IntToBool(b) {
		t.Error("Int b should cast to false")
	}

	tmp2 := 0
	b = &tmp2
	if *IntToBool(b) {
		t.Error("Int b should cast to false")
	}
}

func TestBools(t *testing.T) {
	tmp := true
	var b Bool = &tmp

	if !*BoolToBool(b) {
		t.Error("Bool b should not cast to false")
	}

	var c Bool
	if *BoolToBool(c) {
		t.Error("Bool c should cast to false")
	}

	tmp2 := false
	c = &tmp2
	if *BoolToBool(c) {
		t.Error("Bool c should cast to false")
	}
}

func TestFloat(t *testing.T) {
	tmp := 3.14
	var f Float = &tmp

	if !*FloatToBool(f) {
		t.Error("Float f should not cast to false")
	}

	var b Float
	if *FloatToBool(b) {
		t.Error("Int b should cast to false")
	}

	tmp2 := 0.0
	b = &tmp2
	if *FloatToBool(b) {
		t.Error("Int b should cast to false")
	}
}

func TestString(t *testing.T) {
	tmp := "hello world"
	var s String = &tmp

	if !*StringToBool(s) {
		t.Error("String s should not cast to false")
	}

	var b String
	if *StringToBool(b) {
		t.Error("String s should cast to false")
	}

	tmp2 := ""
	b = &tmp2
	if *StringToBool(b) {
		t.Error("String s should cast to false")
	}
}

func TestCastFloat(t *testing.T) {
	tmp := 3.14
	var f Float = &tmp

	i := FloatToInt(f)
	if *i != 3 {
		t.Error("Float cast to int failed")
	}

	f2 := FloatToFloat(f)
	if *f2 != *f {
		t.Error("Float cast to float failed")
	}
}

func TestCastInt(t *testing.T) {
	tmp := 7
	var i Int = &tmp

	f := IntToFloat(i)
	if *f != 7.0 {
		t.Error("Float cast to float failed")
	}

	i2 := IntToInt(i)
	if *i2 != 7 {
		t.Error("Float cast to int failed")
	}
}
