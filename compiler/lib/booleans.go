package main

type BoolOpt struct {
	val, null bool
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
