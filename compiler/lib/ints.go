package main

type IntOpt struct {
	val  int
	null bool
}

/*
 * ADDING
 */
func (a IntOpt) AddFloat(b FloatOpt) FloatOpt {
	if a.null || b.null {
		return FloatOpt{
			null: true,
		}
	}
	return FloatOpt{
		val: float64(a.val) + b.val,
	}
}
func (a IntOpt) AddInt(b IntOpt) IntOpt {
	if a.null || b.null {
		return IntOpt{
			null: true,
		}
	}
	return IntOpt{
		val: a.val + b.val,
	}
}

/*
 * Subtracting
 */
func (a IntOpt) SubtractFloat(b FloatOpt) FloatOpt {
	if a.null || b.null {
		return FloatOpt{
			null: true,
		}
	}
	return FloatOpt{
		val: float64(a.val) - b.val,
	}
}
func (a IntOpt) SubtractInt(b IntOpt) IntOpt {
	if a.null || b.null {
		return IntOpt{
			null: true,
		}
	}
	return IntOpt{
		val: a.val - b.val,
	}
}

/*
 * Division
 */
func (a IntOpt) DivisionFloat(b FloatOpt) FloatOpt {
	if a.null || b.null {
		return FloatOpt{
			null: true,
		}
	}
	return FloatOpt{
		val: float64(a.val) / b.val,
	}
}
func (a IntOpt) DivisionInt(b IntOpt) IntOpt {
	if a.null || b.null {
		return IntOpt{
			null: true,
		}
	}
	return IntOpt{
		val: a.val / b.val,
	}
}

/*
 * Division
 */
func (a IntOpt) MultiplyFloat(b FloatOpt) FloatOpt {
	if a.null || b.null {
		return FloatOpt{
			null: true,
		}
	}
	return FloatOpt{
		val: float64(a.val) + b.val,
	}
}
func (a IntOpt) MultiplyInt(b IntOpt) IntOpt {
	if a.null || b.null {
		return IntOpt{
			null: true,
		}
	}
	return IntOpt{
		val: a.val * b.val,
	}
}
