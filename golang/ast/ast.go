package ast

import (
	"strings"
)

type Defs []*Def

func (ds Defs) String() string {
	ss := make([]string, len(ds))
	for i, d := range ds {
		ss[i] = d.String()
	}
	return strings.Join(ss, "\n")
}

type Def struct {
	Name string
	Expr *LazyExpr
}

func (d *Def) String() string {
	return "#" + d.Name + " = " + d.Expr.String()
}

type Type int

var (
	Or         = Type(1)
	Concat     = Type(2)
	ZeroOrMore = Type(3)
	Reference  = Type(4)
	Value      = Type(5)
	Interleave = Type(6)
)

type LazyExpr struct {
	f func() *Expr
	e *Expr
}

func (l *LazyExpr) IsLazy() bool {
	return l.e == nil
}

func (l *LazyExpr) Expr() *Expr {
	if l.e != nil {
		return l.e
	}
	e := l.f()
	l.e = e
	return e
}

func (l *LazyExpr) String() string {
	if l.e != nil {
		return l.e.String()
	}
	return "lazy"
}

func NewExpr(e *Expr) *LazyExpr {
	return &LazyExpr{e: e}
}

func NewLazy(f func() *Expr) *LazyExpr {
	return &LazyExpr{f: f}
}

type Expr struct {
	Type  Type
	Expr  *LazyExpr
	Expr2 *LazyExpr
	Value string
}

func (e *Expr) String() string {
	switch e.Type {
	case Value:
		return e.Value
	case Or:
		return "( " + e.Expr.String() + " | " + e.Expr2.String() + " )"
	case Concat:
		return "[" + e.Expr.String() + ", " + e.Expr2.String() + "]"
	case Interleave:
		return "{" + e.Expr.String() + "; " + e.Expr2.String() + "}"
	case ZeroOrMore:
		return "(" + e.Expr.String() + ")*"
	case Reference:
		return "@" + e.Value
	}
	panic("unreachable")
}
