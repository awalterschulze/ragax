package main

import (
	"encoding/json"
	"fmt"
	"github.com/awalterschulze/ragax/golang/deriv"
	"strings"
	"testing"
)

func TestExample1(t *testing.T) {
	var example = `
	#S = ([S, (@R | @D)])*
	#R = [R, (@G | empty)]
	#G = [G, (@P | @R)]
	#P = [P, (@D | @G)]
	#D = [D, (empty | @P)]
`
	if e := EvalToEmptySetNotes(example, []int64{0, 2}); e != "false" {
		t.Fatalf("expected false, but got %s", e)
	}
	if e := EvalToEmptySetNotes(example, []int64{0, 4}); e != "true" {
		t.Fatalf("expected true, but got %s", e)
	}
}

func TestExampleParseError(t *testing.T) {
	var example = `
	#S = ([S, (@R | @D)])*
	#R = [R, (@G | empty)]
	#G = [G, (@P | @R)]
	#P = [P, (@D | @G)]
	#D = [D, (empty 
`
	if e := EvalToEmptySetNotes(example, []int64{0, 2}); !strings.Contains(e, "rror") {
		t.Fatalf("expected error, but got %s", e)
	}
}

func TestParseToJson(t *testing.T) {
	ast, err := deriv.Parse(`
	#S = ([S, (@R | @D)])*
	#R = [R, (@G | empty)]
	#G = [G, (@P | @R)]
	#P = [P, (@D | @G)]
	#D = [D, (empty | @P)]
`)
	if err != nil {
		t.Fatal(err)
	}
	data, err := json.Marshal(ast)
	if err != nil {
		t.Fatal(err)
	}
	fmt.Printf("%s\n", string(data))
}
