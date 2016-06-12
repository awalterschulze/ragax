var inlets = 1;
var outlets = 1;

function getUrlParameter(sParam) {
    var sPageURL = window.location.search.substring(1);
    var sURLVariables = sPageURL.split('&');
    for (var i = 0; i < sURLVariables.length; i++) 
    {
        var sParameterName = sURLVariables[i].split('=');
        if (sParameterName[0] == sParam) 
        {
            return sParameterName[1];
        }
    }
}

function validateCode(mode, codeMirrors) {
	$('#theoutput').prop("class", "alert alert-info");
	$('#theoutput').text("compiling...");
	var exprcode = codeMirrors["expr"].getValue();
	var inputcode = codeMirrors[mode].getValue();
	var validateFunc = gofunctions["Eval"];
	var res = validateFunc(exprcode, inputcode);
	if (res.indexOf("Error: ") === 0) {
		res = res.replace("Error: ", "");
		$('#theoutput').prop("class", "alert alert-danger");
		$('#theoutput').text(res);
	} else {
		if (res == "true") {
			$('#theoutput').prop("class", "alert alert-success");
			$('#theoutput').text("Matches");
		} else if (res == "false") {
			$('#theoutput').prop("class", "alert alert-warning");
			$('#theoutput').text("No Match");
		} else {
			$('#theoutput').prop("class", "alert alert-danger");
			$('#theoutput').text(res);
		}
	}
}

function reportError(err) {
	$('#theoutput').prop("class", "alert alert-danger");
	$('#theoutput').text(err);
}

function setHeightAuto() {
	var codeMirrors = $(".CodeMirror");
	for (var i = 0; i < codeMirrors.length; i++) {
    	codeMirrors[i].style.height = "auto";
	}
}

function setHeightDefault() {
	var codeMirrors = $(".CodeMirror");
	for (var i = 0; i < codeMirrors.length; i++) {
    	codeMirrors[i].style.height = "25em";
	}
}

var defaultExpr = `#S = ([S, (@R | @D)])*
#R = [R, (@G | empty)]
#G = [G, (@P | @R)]
#P = [P, (@D | @G)]
#D = [D, (empty | @P)]

/*
#S = ([@S, [S, (@R | @D)]] | empty)
#R = [R, (@G | empty)]
#G = [G, (@P | @R)]
#P = [P, (@D | @G)]
#D = [D, (empty | @P)]
*/
`;

var defaultInput = `SR`;

function setDefaults(mode, codeMirrors) {
	codeMirrors["expr"].setValue(defaultExpr);
	codeMirrors["input"].setValue(defaultInput);
}

function init() {
	gistText = getUrlParameter("gist");
	share = getUrlParameter("share");
	var exprCodeMirror = CodeMirror(document.getElementById("lefttextarea"), {
  		mode:  "exprmode",
  		value: 'loading...',
  		viewportMargin: Infinity
	});
	var codeMirrors = {"expr": exprCodeMirror};
	var inputCodeMirror = CodeMirror(document.getElementById("righttextarea"), {
  		mode:  {name: "javascript", json: true},
  		value: 'loading...',
  		viewportMargin: Infinity
	});
	codeMirrors["input"] = inputCodeMirror;
	var mode = "input";
	$("#mode" + mode).addClass("active");
	$("#inputheading").text(mode + " input");

	if (gistText == undefined) {
		setDefaults(mode, codeMirrors);
	} else {
		loadCode(codeMirrors);
		if (!(share == undefined)) {
			displayShareBox();
	    }
	}

	$("#saveButton").click(function(ev) { 
		saveCode(mode, codeMirrors);
	});

	$("#validateButton").click(function(ev) { 
		ev.preventDefault();
		validateCode(mode, codeMirrors);
	});

	setHeightDefault();
	$("#autosizeButton").click(function(ev) {
		ev.preventDefault();
		wasChecked = $("#autosizeButton").hasClass("active");
		if (wasChecked) {
			$("#autosizeButton").removeClass("active");
			setHeightDefault();
		} else {
			$("#autosizeButton").addClass("active");
			setHeightAuto();
		}
	});

	for (var key in codeMirrors) {
		codeMirrors[key].on('keyup', function(instance, event) {
    		validateCode(mode, codeMirrors);
		});	
	}
}