package main

type FloatOpt struct {
	val  float64
	null bool
}

/*
 * ADDING
 */
func (a FloatOpt) AddFloat(b FloatOpt) FloatOpt {
	if a.null || b.null {
		return FloatOpt{
			null: true,
		}
	}
	return FloatOpt{
		val: a.val + b.val,
	}
}
func (a FloatOpt) AddInt(b IntOpt) FloatOpt {
	if a.null || b.null {
		return FloatOpt{
			null: true,
		}
	}
	return FloatOpt{
		val: a.val + float64(b.val),
	}
}

/*
 * Subtracting
 */
func (a FloatOpt) SubtractFloat(b FloatOpt) FloatOpt {
	if a.null || b.null {
		return FloatOpt{
			null: true,
		}
	}
	return FloatOpt{
		val: a.val - b.val,
	}
}
func (a FloatOpt) SubtractInt(b IntOpt) FloatOpt {
	if a.null || b.null {
		return FloatOpt{
			null: true,
		}
	}
	return FloatOpt{
		val: a.val - float64(b.val),
	}
}

/*
 * Division
 */
func (a FloatOpt) DivisionFloat(b FloatOpt) FloatOpt {
	if a.null || b.null {
		return FloatOpt{
			null: true,
		}
	}
	return FloatOpt{
		val: a.val / b.val,
	}
}
func (a FloatOpt) DivisionInt(b IntOpt) FloatOpt {
	if a.null || b.null {
		return FloatOpt{
			null: true,
		}
	}
	return FloatOpt{
		val: a.val / float64(b.val),
	}
}

/*
 * Division
 */
func (a FloatOpt) MultiplyFloat(b FloatOpt) FloatOpt {
	if a.null || b.null {
		return FloatOpt{
			null: true,
		}
	}
	return FloatOpt{
		val: a.val * b.val,
	}
}
func (a FloatOpt) MultiplyInt(b IntOpt) FloatOpt {
	if a.null || b.null {
		return FloatOpt{
			null: true,
		}
	}
	return FloatOpt{
		val: a.val * float64(b.val),
	}
}
