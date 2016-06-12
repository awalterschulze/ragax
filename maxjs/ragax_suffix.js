

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
