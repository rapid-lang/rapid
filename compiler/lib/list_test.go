package main

import "testing"

func TestListAcces(t *testing.T) {
  tmp1 := 1
  a := Int(&tmp1)
  tmp2 := 2
  b := Int(&tmp2)
  tmp3 := 3
  c := Int(&tmp3)

  lst := []Int{a, b, c}

  for _, element := range lst {
    if element == nil {
      t.Error("List element should not be NULL")
    } else if !(*element > 0) {
      t.Error("List elements should all be > 0")
    }
  }
}
