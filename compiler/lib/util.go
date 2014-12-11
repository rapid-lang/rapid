package main

import "regexp"

var (
	// hax
	re = regexp.MustCompile(`%\w`) // match format statement to be replaced
)

// ProperFormatString used to replace all instances of print
// formats with %s since we rely on the Stringer interface
func properFormatString(f string) string {
	return re.ReplaceAllString(f, "%s")
}
