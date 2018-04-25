// Code generated by gocc; DO NOT EDIT.

package parser

import (
  . "github.com/awalterschulze/ragax/golang/ast"
  "github.com/awalterschulze/ragax/golang/token"
)

func newString(v interface{}) string {
  t := v.(*token.Token)
  return string(t.Lit)
}

type (
	//TODO: change type and variable names to be consistent with other tables
	ProdTab      [numProductions]ProdTabEntry
	ProdTabEntry struct {
		String     string
		Id         string
		NTType     int
		Index      int
		NumSymbols int
		ReduceFunc func([]Attrib) (Attrib, error)
	}
	Attrib interface {
	}
)

var productionsTable = ProdTab{
	ProdTabEntry{
		String: `S' : Definitions	<<  >>`,
		Id:         "S'",
		NTType:     0,
		Index:      0,
		NumSymbols: 1,
		ReduceFunc: func(X []Attrib) (Attrib, error) {
			return X[0], nil
		},
	},
	ProdTabEntry{
		String: `Definitions : Definition	<< Defs{X[0].(*Def)}, nil >>`,
		Id:         "Definitions",
		NTType:     1,
		Index:      1,
		NumSymbols: 1,
		ReduceFunc: func(X []Attrib) (Attrib, error) {
			return Defs{X[0].(*Def)}, nil
		},
	},
	ProdTabEntry{
		String: `Definitions : Definitions Definition	<< append(X[0].(Defs), X[1].(*Def)), nil >>`,
		Id:         "Definitions",
		NTType:     1,
		Index:      2,
		NumSymbols: 2,
		ReduceFunc: func(X []Attrib) (Attrib, error) {
			return append(X[0].(Defs), X[1].(*Def)), nil
		},
	},
	ProdTabEntry{
		String: `Definition : "#" id "=" Expr	<< &Def{Name: newString(X[1]), Expr: NewExpr(X[3].(*Expr))}, nil >>`,
		Id:         "Definition",
		NTType:     2,
		Index:      3,
		NumSymbols: 4,
		ReduceFunc: func(X []Attrib) (Attrib, error) {
			return &Def{Name: newString(X[1]), Expr: NewExpr(X[3].(*Expr))}, nil
		},
	},
	ProdTabEntry{
		String: `Expr : "(" Expr "|" Expr ")"	<< &Expr{
		Type: Or, 
		Expr: NewExpr(X[1].(*Expr)), 
		Expr2: NewExpr(X[3].(*Expr)),
	}, nil >>`,
		Id:         "Expr",
		NTType:     3,
		Index:      4,
		NumSymbols: 5,
		ReduceFunc: func(X []Attrib) (Attrib, error) {
			return &Expr{
		Type: Or, 
		Expr: NewExpr(X[1].(*Expr)), 
		Expr2: NewExpr(X[3].(*Expr)),
	}, nil
		},
	},
	ProdTabEntry{
		String: `Expr : "[" Expr "," Expr "]"	<< &Expr{
		Type: Concat, 
		Expr: NewExpr(X[1].(*Expr)), 
		Expr2: NewExpr(X[3].(*Expr)),
	}, nil >>`,
		Id:         "Expr",
		NTType:     3,
		Index:      5,
		NumSymbols: 5,
		ReduceFunc: func(X []Attrib) (Attrib, error) {
			return &Expr{
		Type: Concat, 
		Expr: NewExpr(X[1].(*Expr)), 
		Expr2: NewExpr(X[3].(*Expr)),
	}, nil
		},
	},
	ProdTabEntry{
		String: `Expr : "{" Expr ";" Expr "}"	<< &Expr{
		Type: Interleave, 
		Expr: NewExpr(X[1].(*Expr)), 
		Expr2: NewExpr(X[3].(*Expr)),
	}, nil >>`,
		Id:         "Expr",
		NTType:     3,
		Index:      6,
		NumSymbols: 5,
		ReduceFunc: func(X []Attrib) (Attrib, error) {
			return &Expr{
		Type: Interleave, 
		Expr: NewExpr(X[1].(*Expr)), 
		Expr2: NewExpr(X[3].(*Expr)),
	}, nil
		},
	},
	ProdTabEntry{
		String: `Expr : "(" Expr ")" "*"	<< &Expr{
		Type: ZeroOrMore, 
		Expr: NewExpr(X[1].(*Expr)),
	}, nil >>`,
		Id:         "Expr",
		NTType:     3,
		Index:      7,
		NumSymbols: 4,
		ReduceFunc: func(X []Attrib) (Attrib, error) {
			return &Expr{
		Type: ZeroOrMore, 
		Expr: NewExpr(X[1].(*Expr)),
	}, nil
		},
	},
	ProdTabEntry{
		String: `Expr : "@" id	<< &Expr{
		Type: Reference, 
		Value: newString(X[1]),
	}, nil >>`,
		Id:         "Expr",
		NTType:     3,
		Index:      8,
		NumSymbols: 2,
		ReduceFunc: func(X []Attrib) (Attrib, error) {
			return &Expr{
		Type: Reference, 
		Value: newString(X[1]),
	}, nil
		},
	},
	ProdTabEntry{
		String: `Expr : id	<< &Expr{
		Type: Value, 
		Value: newString(X[0]),
	}, nil >>`,
		Id:         "Expr",
		NTType:     3,
		Index:      9,
		NumSymbols: 1,
		ReduceFunc: func(X []Attrib) (Attrib, error) {
			return &Expr{
		Type: Value, 
		Value: newString(X[0]),
	}, nil
		},
	},
}
