
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
