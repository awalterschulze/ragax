package deriv

import (
	"fmt"
	. "github.com/awalterschulze/ragax/golang/ast"
	"testing"
)

var example = `
	#S = ([S, (@R | @D)])*
	#R = [R, (@G | empty)]
	#G = [G, (@P | @R)]
	#P = [P, (@D | @G)]
	#D = [D, (empty | @P)]
`

func TestExample1Parse(t *testing.T) {
	defs, err := Parse(example)
	if err != nil {
		t.Fatal(err)
	}
	if len(defs) != 5 {
		t.Fatalf("expected 5 defs got %d", len(defs))
	}
	if defs[0].Name != "S" {
		t.Fatalf("expected first def S, but got %s", defs[0].Name)
	}
}

func TestExample1Deriv(t *testing.T) {
	defs, err := Parse(example)
	if err != nil {
		t.Fatal(err)
	}

	pos := []string{``, `SR`, `SRGPDPGR`, `SRGR`, `SRGRGRGRGRGRGPGR`}
	for _, p := range pos {
		fmt.Printf("--- TESTING %s\n", p)
		if !Nullable(defs, NewExpr(Eval(defs, p))) {
			t.Fatalf("=== FAIL expected match for `%s`", p)
		} else {
			t.Logf("=== PASS")
		}
	}

	neg := []string{`SG`, `SRGPDDPGRS`, `SRGS`, `SRGRGRGRGRGRGDGRS`}
	for _, n := range neg {
		fmt.Printf("--- TESTING %s\n", n)
		res := Eval(defs, n)
		if Nullable(defs, NewExpr(res)) {
			t.Fatalf("=== FAIL expected no match for `%s`", n)
		} else {
			res = Simplify(defs, res)
			if res.Type == Value && res.Value == "emptyset" {
				t.Logf("=== PASS")
			} else {
				t.Fatalf("Expected emptyset, but got %s", res)
			}
		}
	}
}
