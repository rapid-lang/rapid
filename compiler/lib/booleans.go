package main

import "fmt"

type BoolOpt struct {
	val, null bool
}

func (b BoolOpt) String() string {
	if b.null {
		return "NULL"
	}
	return fmt.Sprintf("%t", b.val)
}

/*
 * Evaluation
 */
func (a BoolOpt) Equality(b BoolOpt) BoolOpt {
	if a.null || b.null {
		return BoolOpt{
			null: true,
		}
	}
	return BoolOpt{
		val: a.val && b.val,
	}
}

func (a BoolOpt) Or(b BoolOpt) BoolOpt {
	if a.null || b.null {
		return BoolOpt{
			null: true,
		}
	}
	return BoolOpt{
		val: a.val || b.val,
	}
}
