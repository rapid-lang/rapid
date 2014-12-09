package main

import "fmt"

type FloatOpt struct {
	val  float64
	null bool
}

func (f FloatOpt) String() string {
	if f.null {
		return "NULL"
	}
	return fmt.Sprintf("%f", f.val)
}

func Float(i IntOpt) FloatOpt {
	if i.null {
		return FloatOpt{
			null: true,
		}
	}
	return FloatOpt{
		val: float64(i.val),
	}
}

func (a FloatOpt) Equal(b FloatOpt) BoolOpt {
	if a.null && b.null {
		return BoolOpt{
			val: true,
		}
	}
	if a.null || b.null {
		return BoolOpt{
			null: true,
		}
	}
	return BoolOpt{
		val: a.val == b.val,
	}
}

func (a FloatOpt) Add(b FloatOpt) FloatOpt {
	if a.null || b.null {
		return FloatOpt{
			null: true,
		}
	}
	return FloatOpt{
		val: a.val + b.val,
	}
}

func (a FloatOpt) Subtract(b FloatOpt) FloatOpt {
	if a.null || b.null {
		return FloatOpt{
			null: true,
		}
	}
	return FloatOpt{
		val: a.val - b.val,
	}
}

func (a FloatOpt) Divide(b FloatOpt) FloatOpt {
	if a.null || b.null {
		return FloatOpt{
			null: true,
		}
	}
	return FloatOpt{
		val: a.val / b.val,
	}
}

func (a FloatOpt) Multiply(b FloatOpt) FloatOpt {
	if a.null || b.null {
		return FloatOpt{
			null: true,
		}
	}
	return FloatOpt{
		val: a.val * b.val,
	}
}
