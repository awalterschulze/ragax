package deriv

import (
	"fmt"
	. "github.com/awalterschulze/ragax/golang/ast"
	"github.com/awalterschulze/ragax/golang/lexer"
	"github.com/awalterschulze/ragax/golang/parser"
)

func Parse(s string) (Defs, error) {
	p := parser.NewParser()
	l := lexer.NewLexer([]byte(s))
	r, err := p.Parse(l)
	if err != nil {
		return nil, err
	}
	return r.(Defs), nil
}

func Eval(defs Defs, s string) *Expr {
	d := defs[0].Expr.Expr()
	mem := make(Mem)
	deriv := &deriv{mem: mem, defs: defs}
	for _, c := range s {
		fmt.Printf("-- %s deriv %s\n", d, string([]rune{c}))
		d = deriv.Deriv(d, c)
		fmt.Printf("== res %s\n", d)
	}
	return d
}

var emptyset = &Expr{Type: Value, Value: "emptyset"}
var empty = &Expr{Type: Value, Value: "empty"}

type Mem map[*Expr]map[rune]*Expr

func (m Mem) Get(e *Expr, r rune) *Expr {
	mm, ok := m[e]
	if ok {
		return mm[r]
	}
	return nil
}

func (m Mem) Set(e *Expr, r rune, v *Expr) *Expr {
	if _, ok := m[e]; !ok {
		m[e] = make(map[rune]*Expr)
	}
	m[e][r] = v
	return v
}

type deriv struct {
	mem  Mem
	defs Defs
}

func (this *deriv) Sderiv(e *Expr, c rune) *Expr {
	return Simplify(this.defs, this.Deriv(e, c))
}

func (this *deriv) Deriv(e *Expr, c rune) *Expr {
	if res := this.mem.Get(e, c); res != nil {
		return res
	}
	fmt.Printf("%s deriv %v\n", e, string([]rune{c}))
	switch e.Type {
	case Value:
		switch e.Value {
		case "empty":
			return this.mem.Set(e, c, emptyset)
		case "emptyset":
			return this.mem.Set(e, c, emptyset)
		default:
			if e.Value == string([]rune{c}) {
				return this.mem.Set(e, c, empty)
			}
			return this.mem.Set(e, c, emptyset)
		}
	case Or:
		return this.mem.Set(e, c, &Expr{
			Type: Or,
			Expr: NewLazy(func() *Expr {
				return this.Sderiv(e.Expr.Expr(), c)
			}),
			Expr2: NewLazy(func() *Expr {
				return this.Sderiv(e.Expr2.Expr(), c)
			}),
		})
	case Concat:
		if Nullable(this.defs, e.Expr) {
			return this.mem.Set(e, c, &Expr{
				Type: Or,
				Expr: NewExpr(&Expr{
					Type: Concat,
					Expr: NewLazy(func() *Expr {
						return this.Sderiv(e.Expr.Expr(), c)
					}),
					Expr2: e.Expr2,
				}),
				Expr2: NewLazy(func() *Expr {
					return this.Sderiv(e.Expr2.Expr(), c)
				}),
			})
		} else {
			return this.mem.Set(e, c, &Expr{
				Type: Concat,
				Expr: NewLazy(func() *Expr {
					return this.Sderiv(e.Expr.Expr(), c)
				}),
				Expr2: e.Expr2,
			})
		}
	case ZeroOrMore:
		return this.mem.Set(e, c, &Expr{
			Type: Concat,
			Expr: NewLazy(func() *Expr {
				return this.Sderiv(e.Expr.Expr(), c)
			}),
			Expr2: NewExpr(Simplify(this.defs, &Expr{Type: ZeroOrMore, Expr: e.Expr})),
		})
	case Reference:
		expr1 := lookup(this.defs, e.Value).Expr()
		return this.mem.Set(e, c, this.Deriv(expr1, c))
	}
	panic("unreachable")
}

func lookup(defs Defs, name string) *LazyExpr {
	for _, def := range defs {
		if def.Name == name {
			return def.Expr
		}
	}
	return nil
}

type nullable struct {
	defs    Defs
	mem     map[*LazyExpr]bool
	visited map[*LazyExpr]bool
}

func (this *nullable) Nullable(l *LazyExpr) bool {
	if m, ok := this.mem[l]; ok {
		return m
	}
	if v := this.visited[l]; v {
		this.mem[l] = false
		return false
	}
	this.visited[l] = true
	e := l.Expr()
	switch e.Type {
	case Value:
		switch e.Value {
		case "empty":
			this.mem[l] = true
			return true
		case "emptyset":
			this.mem[l] = false
			return false
		default:
			this.mem[l] = false
			return false
		}
	case Or:
		v := this.Nullable(e.Expr) || this.Nullable(e.Expr2)
		this.mem[l] = v
		return v
	case Concat:
		v := this.Nullable(e.Expr) && this.Nullable(e.Expr2)
		this.mem[l] = v
		return v
	case ZeroOrMore:
		this.mem[l] = true
		return true
	case Reference:
		v := this.Nullable(lookup(this.defs, e.Value))
		this.mem[l] = v
		return v
	}
	panic(fmt.Sprintf("unreachable %#v", e))
}

func Nullable(defs Defs, l *LazyExpr) bool {
	n := &nullable{
		defs:    defs,
		mem:     make(map[*LazyExpr]bool),
		visited: make(map[*LazyExpr]bool),
	}
	return n.Nullable(l)
}

func isEmptySet(e *Expr) bool {
	return e.Type == Value && e.Value == "emptyset"
}

func isEmpty(e *Expr) bool {
	return e.Type == Value && e.Value == "empty"
}

func Simplify(defs Defs, e *Expr) *Expr {
	switch e.Type {
	case Value:
		return e
	case Or:
		if e.Expr.IsLazy() || e.Expr2.IsLazy() {
			return e
		}
		expr1 := Simplify(defs, e.Expr.Expr())
		expr2 := Simplify(defs, e.Expr2.Expr())
		if isEmptySet(expr1) {
			return expr2
		}
		if isEmptySet(expr2) {
			return expr1
		}
		return &Expr{Type: Or, Expr: NewExpr(expr1), Expr2: NewExpr(expr2)}
	case Concat:
		if e.Expr.IsLazy() || e.Expr2.IsLazy() {
			return e
		}
		expr1 := Simplify(defs, e.Expr.Expr())
		expr2 := Simplify(defs, e.Expr2.Expr())
		if isEmptySet(expr1) {
			return emptyset
		}
		if isEmpty(expr1) {
			return expr2
		}
		if isEmptySet(expr2) {
			return emptyset
		}
		return &Expr{Type: Concat, Expr: NewExpr(expr1), Expr2: NewExpr(expr2)}
	case ZeroOrMore:
		if e.Expr.IsLazy() {
			return e
		}
		expr1 := Simplify(defs, e.Expr.Expr())
		if isEmptySet(expr1) {
			return empty
		}
		return &Expr{Type: ZeroOrMore, Expr: NewExpr(expr1)}
	case Reference:
		return e
	}
	panic(fmt.Sprintf("unreachable %#v", e))
}
