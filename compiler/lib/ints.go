package main

import "fmt"

type IntOpt struct {
	val  int
	null bool
}

func (i IntOpt) String() string {
	if i.null {
		return "NULL"
	}
	return fmt.Sprintf("%d", i.val)
}

func Int(f FloatOpt) IntOpt {
	if f.null {
		return IntOpt{
			null: true,
		}
	}
	return IntOpt{
		val: int(f.val),
	}
}

func (a IntOpt) Equal(b IntOpt) BoolOpt {
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

func (a IntOpt) Add(b IntOpt) IntOpt {
	if a.null || b.null {
		return IntOpt{
			null: true,
		}
	}
	return IntOpt{
		val: a.val + b.val,
	}
}

func (a IntOpt) Subtract(b IntOpt) IntOpt {
	if a.null || b.null {
		return IntOpt{
			null: true,
		}
	}
	return IntOpt{
		val: a.val - b.val,
	}
}

func (a IntOpt) Divide(b IntOpt) IntOpt {
	if a.null || b.null {
		return IntOpt{
			null: true,
		}
	}
	return IntOpt{
		val: a.val / b.val,
	}
}

func (a IntOpt) Multiply(b IntOpt) IntOpt {
	if a.null || b.null {
		return IntOpt{
			null: true,
		}
	}
	return IntOpt{
		val: a.val * b.val,
	}
}
