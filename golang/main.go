package main

import (
	"fmt"
	"github.com/awalterschulze/ragax/golang/ast"
	"github.com/awalterschulze/ragax/golang/deriv"
	"github.com/gopherjs/gopherjs/js"
)

func main() {
	js.Global.Set("gofunctions", map[string]interface{}{
		"EvalToEmptySet":      EvalToEmptySet,
		"EvalToEmptySetNotes": EvalToEmptySetNotes,
		"Eval":                Eval,
	})
}

func midiToNote(v int64) rune {
	switch v % 12 {
	case 0:
		return 'S'
	case 1:
		return 'r'
	case 2:
		return 'R'
	case 3:
		return 'g'
	case 4:
		return 'G'
	case 5:
		return 'm'
	case 6:
		return 'M'
	case 7:
		return 'P'
	case 8:
		return 'd'
	case 9:
		return 'D'
	case 10:
		return 'n'
	case 11:
		return 'N'
	}
	panic("unreachable")
}

func midiToNotes(inputs []int64) string {
	rs := make([]rune, len(inputs))
	for i, input := range inputs {
		rs[i] = midiToNote(input)
	}
	return string(rs)
}

func Eval(g string, input string) string {
	defs, err := deriv.Parse(g)
	if err != nil {
		return err.Error()
	}
	res := deriv.Eval(defs, input)
	if deriv.Nullable(defs, ast.NewExpr(res)) {
		return "true"
	}
	return "false"
}

func EvalToEmptySet(g string, input string) string {
	defs, err := deriv.Parse(g)
	if err != nil {
		return err.Error()
	}
	res := deriv.Eval(defs, input)
	deriv.Nullable(defs, ast.NewExpr(res))
	res = deriv.Simplify(defs, res)
	fmt.Printf("final = %s\n", res)
	if res.Type == ast.Value && res.Value == "emptyset" {
		return "true"
	}
	return "false"
}

func EvalToEmptySetNotes(g string, is []int64) string {
	input := midiToNotes(is)
	return EvalToEmptySet(g, input)
}
