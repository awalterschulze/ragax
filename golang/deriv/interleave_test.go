package deriv

import (
	"fmt"
	"testing"

	. "github.com/awalterschulze/ragax/golang/ast"
)

var interleaveStr = `
#start = { @ab ; @cd }
#ab = ([A, [@start, [B, @start]]] | empty)
#cd = ([C, [@start, [D, @start]]] | empty)
`

func TestLInterleave(t *testing.T) {
	defs, err := Parse(interleaveStr)
	if err != nil {
		t.Fatal(err)
	}

	pos := []string{`ABABAABB`, `ABCD`, `ACBD`, `CCADDB`}
	for _, p := range pos {
		fmt.Printf("--- TESTING %s\n", p)
		if !Nullable(defs, NewExpr(Eval(defs, p))) {
			t.Fatalf("=== FAIL expected match for `%s`", p)
		} else {
			t.Logf("=== PASS")
		}
	}

	neg := []string{`BA`, `DC`, `A`}
	for _, n := range neg {
		fmt.Printf("--- TESTING %s\n", n)
		if Nullable(defs, NewExpr(Eval(defs, n))) {
			t.Fatalf("=== FAIL expected no match for `%s`", n)
		} else {
			t.Logf("=== PASS")
		}
	}
}
