
// inlets and outlets
inlets = 1;
outlets = 1;

// global variables
var exampleDefs = {
	"S": many(concat(value("S"), or(ref("R"), ref("D")))),
	"R": concat(value("R"), or(ref("G"), empty())),
	"G": concat(value("G") ,or(ref("P"), ref("R"))),
	"P": concat(value("P"), or(ref("D"), ref("G"))),
	"D": concat(value("D"), or(empty(), ref("P")))
};

var currentState = exampleDefs["S"];

function many(e) {
	return {"Type": "ZeroOrMore", "Expr": e}
}

function concat(e1, e2) {
	return {"Type": "Concat", "Expr": e1, "Expr2": e2}
}

function or(e1, e2) {
	return {"Type": "Or", "Expr": e1, "Expr2": e2}
}

function value(v) {
	return {"Type": "Value", "Value": v}
}

function ref(v) {
	return {"Type": "Reference", "Name": v}
}

function emptyset() {
	return {"Type": "Value", "Value": "emptyset"}
} 

function empty() {
	return {"Type": "Value", "Value": "empty"}
}

function nullable(refs, expr) {
	switch (expr.Type) {
	case "Value":
		switch (expr.Value) {
		case "empty":
			return true
		case "emptyset":
			return false
		}
		return false
	case "Or":
		return nullable(refs, expr.Expr) || nullable(refs, expr.Expr2)
	case "Concat":
		return nullable(refs, expr.Expr) && nullable(refs, expr.Expr2)
	case "ZeroOrMore":
		return true
	case "Reference":
		return nullable(refs, refs[expr.Name])
	default:
		console.log("unknown type: " + expr.Type)
	}
}

function toStr(expr) {
	switch (expr.Type) {
	case "Value":
		return "\"" + expr.Value + "\""
	case "Or":
		return "or(" + toStr(expr.Expr) + ", " + toStr(expr.Expr2) + ")"
	case "Concat":
		return "concat(" + toStr(expr.Expr) + ", " + toStr(expr.Expr2) + ")"
	case "ZeroOrMore":
		return "many(" + toStr(expr.Expr) + ")"
	case "Reference":
		return "ref(" + expr.Name + ")"
	default:
		throw("unknown type: " + JSON.stringify(expr))
	}
}

function isEmpty(expr) {
	return (expr.Type == "Value") && (expr.Value == "empty")
}

function isEmptySet(expr) {
	return (expr.Type == "Value") && (expr.Value == "emptyset")
}

function simplify(expr) {
	switch (expr.Type) {
	case "Value":
		return expr
	case "Or":
		var left = simplify(expr.Expr);
		var right = simplify(expr.Expr2);
		if (isEmptySet(left)) {
			return right
		}
		if (isEmptySet(right)) {
			return left
		}
		if (JSON.stringify(left) == JSON.stringify(right)) {
			return left
		}
		return or(left, right)
	case "Concat":
		var left = simplify(expr.Expr);
		var right = simplify(expr.Expr2);
		if (isEmpty(left)) {
			return right
		}
		if (isEmptySet(left)) {
			return emptyset()
		}
		if (isEmptySet(right)) {
			return emptyset()
		}
		return concat(left, right)
	case "ZeroOrMore":
		return many(simplify(expr.Expr))
	case "Reference":
		return expr
	default:
		throw("unknown type: " + JSON.stringify(expr))
	}
}

function satis(refs, expr) {
	switch (expr.Type) {
	case "Value":
		switch (expr.Value) {
		case "empty":
			return true
		case "emptyset":
			return false
		}
		return true
	case "Or":
		return satis(refs, expr.Expr) || satis(refs, expr.Expr2)
	case "Concat":
		return satis(refs, expr.Expr) && satis(refs, expr.Expr2)
	case "ZeroOrMore":
		return true
	case "Reference":
		return satis(refs, refs[expr.Name])
	default:
		throw("unknown type: " + JSON.stringify(expr))
	}
}

function deriv(refs, expr, c) {
	switch (expr.Type) {
	case "Value":
		switch (expr.Value) {
		case "empty":
			return emptyset()
		case "emptyset":
			return emptyset()
		}
		if (expr.Value == c) {
			return empty()
		} else {
			return emptyset()
		}
	case "Or":
		var left = deriv(refs, expr.Expr, c);
		var right = deriv(refs, expr.Expr2, c);
		return or(left, right)
	case "Concat":
		if (nullable(refs, expr.Expr)) {
			var left = deriv(refs, expr.Expr, c);
			var right = deriv(refs, expr.Expr2, c);
			return or(concat(left, expr.Expr2), right)
		} else {
			var left = deriv(refs, expr.Expr, c);
			return concat(left, expr.Expr2)
		}
	case "ZeroOrMore":
		var e = deriv(refs, expr.Expr, c);
		return concat(e, expr)
	case "Reference":
		return deriv(refs, refs[expr.Name], c)
	default:
		throw("unknown type: " + JSON.stringify(expr))
	}
}



function midiToNote(v) {
	switch (v % 12) {
	case 0:
		return 'S';
	case 1:
		return 'r';
	case 2:
		return 'R';
	case 3:
		return 'g';
	case 4:
		return 'G';
	case 5:
		return 'm';
	case 6:
		return 'M';
	case 7:
		return 'P';
	case 8:
		return 'd';
	case 9:
		return 'D';
	case 10:
		return 'n';
	case 11:
		return 'N';
	default:
		return 'X';
	}
}

function msg_int(note)
{
	var notestr = midiToNote(note);
	var newState = deriv(exampleDefs, currentState, notestr);
	newState = simplify(newState);
	if (satis(exampleDefs, newState)) {
		currentState = newState;
	} else {
		note = 0;
	}
	outlet(0, note);
}
