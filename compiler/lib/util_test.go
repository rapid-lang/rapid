package main

import "testing"

func TestProperFormatString(t *testing.T) {
	tests := []struct {
		input, output string
	}{
		{"%w", "%s"},
		{"0%w", "0%s"},
		{"%d", "%s"},
		{"%d%d", "%s%s"},
		{"%d%d%d", "%s%s%s"},
		{"%d %d%d", "%s %s%s"},
		// NOTE: we don't leave the ability to use format to build strings so the case
		// of using "%%" is not handled as it would be otherwise
		{"%%v", "%%s"},
	}

	for _, test := range tests {
		res := properFormatString(test.input)
		if res != test.output {
			t.Errorf("Expected: [%s], Found: [%s]", test.output, res)
		}
	}
}
