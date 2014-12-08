package main

type StringOpt struct {
	val  string
	null bool
}

func (s StringOpt) String() string {
	if s.null {
		return "NULL"
	}
	return s.val
}

/*
 * Equality
 */
func (a StringOpt) EqualInt(b StringOpt) BoolOpt {
	if a.null || b.null {
		return BoolOpt{
			null: true,
		}
	}
	return BoolOpt{
		val: a.val == b.val,
	}
}

/*
 * Concatenating
 */
func (a StringOpt) AddFloat(b StringOpt) StringOpt {
	if a.null || b.null {
		return StringOpt{
			null: true,
		}
	}
	return StringOpt{
		val: a.val + b.val,
	}
}
