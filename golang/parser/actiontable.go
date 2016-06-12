
// generated by gocc; DO NOT EDIT.

package parser

type(
	actionTable [numStates]actionRow
	actionRow struct {
		canRecover bool
		actions [numSymbols]action
	}
)

var actionTab = actionTable{
	actionRow{ // S0
				canRecover: false,
		actions: [numSymbols]action{
			nil,		/* INVALID */
			nil,		/* $ */
			shift(3),		/* # */
			nil,		/* id */
			nil,		/* = */
			nil,		/* ( */
			nil,		/* | */
			nil,		/* ) */
			nil,		/* [ */
			nil,		/* , */
			nil,		/* ] */
			nil,		/* * */
			nil,		/* @ */
			
		},

	},
	actionRow{ // S1
				canRecover: false,
		actions: [numSymbols]action{
			nil,		/* INVALID */
			accept(true),		/* $ */
			shift(3),		/* # */
			nil,		/* id */
			nil,		/* = */
			nil,		/* ( */
			nil,		/* | */
			nil,		/* ) */
			nil,		/* [ */
			nil,		/* , */
			nil,		/* ] */
			nil,		/* * */
			nil,		/* @ */
			
		},

	},
	actionRow{ // S2
				canRecover: false,
		actions: [numSymbols]action{
			nil,		/* INVALID */
			reduce(1),		/* $, reduce: Definitions */
			reduce(1),		/* #, reduce: Definitions */
			nil,		/* id */
			nil,		/* = */
			nil,		/* ( */
			nil,		/* | */
			nil,		/* ) */
			nil,		/* [ */
			nil,		/* , */
			nil,		/* ] */
			nil,		/* * */
			nil,		/* @ */
			
		},

	},
	actionRow{ // S3
				canRecover: false,
		actions: [numSymbols]action{
			nil,		/* INVALID */
			nil,		/* $ */
			nil,		/* # */
			shift(5),		/* id */
			nil,		/* = */
			nil,		/* ( */
			nil,		/* | */
			nil,		/* ) */
			nil,		/* [ */
			nil,		/* , */
			nil,		/* ] */
			nil,		/* * */
			nil,		/* @ */
			
		},

	},
	actionRow{ // S4
				canRecover: false,
		actions: [numSymbols]action{
			nil,		/* INVALID */
			reduce(2),		/* $, reduce: Definitions */
			reduce(2),		/* #, reduce: Definitions */
			nil,		/* id */
			nil,		/* = */
			nil,		/* ( */
			nil,		/* | */
			nil,		/* ) */
			nil,		/* [ */
			nil,		/* , */
			nil,		/* ] */
			nil,		/* * */
			nil,		/* @ */
			
		},

	},
	actionRow{ // S5
				canRecover: false,
		actions: [numSymbols]action{
			nil,		/* INVALID */
			nil,		/* $ */
			nil,		/* # */
			nil,		/* id */
			shift(6),		/* = */
			nil,		/* ( */
			nil,		/* | */
			nil,		/* ) */
			nil,		/* [ */
			nil,		/* , */
			nil,		/* ] */
			nil,		/* * */
			nil,		/* @ */
			
		},

	},
	actionRow{ // S6
				canRecover: false,
		actions: [numSymbols]action{
			nil,		/* INVALID */
			nil,		/* $ */
			nil,		/* # */
			shift(7),		/* id */
			nil,		/* = */
			shift(9),		/* ( */
			nil,		/* | */
			nil,		/* ) */
			shift(10),		/* [ */
			nil,		/* , */
			nil,		/* ] */
			nil,		/* * */
			shift(11),		/* @ */
			
		},

	},
	actionRow{ // S7
				canRecover: false,
		actions: [numSymbols]action{
			nil,		/* INVALID */
			reduce(8),		/* $, reduce: Expr */
			reduce(8),		/* #, reduce: Expr */
			nil,		/* id */
			nil,		/* = */
			nil,		/* ( */
			nil,		/* | */
			nil,		/* ) */
			nil,		/* [ */
			nil,		/* , */
			nil,		/* ] */
			nil,		/* * */
			nil,		/* @ */
			
		},

	},
	actionRow{ // S8
				canRecover: false,
		actions: [numSymbols]action{
			nil,		/* INVALID */
			reduce(3),		/* $, reduce: Definition */
			reduce(3),		/* #, reduce: Definition */
			nil,		/* id */
			nil,		/* = */
			nil,		/* ( */
			nil,		/* | */
			nil,		/* ) */
			nil,		/* [ */
			nil,		/* , */
			nil,		/* ] */
			nil,		/* * */
			nil,		/* @ */
			
		},

	},
	actionRow{ // S9
				canRecover: false,
		actions: [numSymbols]action{
			nil,		/* INVALID */
			nil,		/* $ */
			nil,		/* # */
			shift(12),		/* id */
			nil,		/* = */
			shift(14),		/* ( */
			nil,		/* | */
			nil,		/* ) */
			shift(15),		/* [ */
			nil,		/* , */
			nil,		/* ] */
			nil,		/* * */
			shift(16),		/* @ */
			
		},

	},
	actionRow{ // S10
				canRecover: false,
		actions: [numSymbols]action{
			nil,		/* INVALID */
			nil,		/* $ */
			nil,		/* # */
			shift(17),		/* id */
			nil,		/* = */
			shift(19),		/* ( */
			nil,		/* | */
			nil,		/* ) */
			shift(20),		/* [ */
			nil,		/* , */
			nil,		/* ] */
			nil,		/* * */
			shift(21),		/* @ */
			
		},

	},
	actionRow{ // S11
				canRecover: false,
		actions: [numSymbols]action{
			nil,		/* INVALID */
			nil,		/* $ */
			nil,		/* # */
			shift(22),		/* id */
			nil,		/* = */
			nil,		/* ( */
			nil,		/* | */
			nil,		/* ) */
			nil,		/* [ */
			nil,		/* , */
			nil,		/* ] */
			nil,		/* * */
			nil,		/* @ */
			
		},

	},
	actionRow{ // S12
				canRecover: false,
		actions: [numSymbols]action{
			nil,		/* INVALID */
			nil,		/* $ */
			nil,		/* # */
			nil,		/* id */
			nil,		/* = */
			nil,		/* ( */
			reduce(8),		/* |, reduce: Expr */
			reduce(8),		/* ), reduce: Expr */
			nil,		/* [ */
			nil,		/* , */
			nil,		/* ] */
			nil,		/* * */
			nil,		/* @ */
			
		},

	},
	actionRow{ // S13
				canRecover: false,
		actions: [numSymbols]action{
			nil,		/* INVALID */
			nil,		/* $ */
			nil,		/* # */
			nil,		/* id */
			nil,		/* = */
			nil,		/* ( */
			shift(23),		/* | */
			shift(24),		/* ) */
			nil,		/* [ */
			nil,		/* , */
			nil,		/* ] */
			nil,		/* * */
			nil,		/* @ */
			
		},

	},
	actionRow{ // S14
				canRecover: false,
		actions: [numSymbols]action{
			nil,		/* INVALID */
			nil,		/* $ */
			nil,		/* # */
			shift(12),		/* id */
			nil,		/* = */
			shift(14),		/* ( */
			nil,		/* | */
			nil,		/* ) */
			shift(15),		/* [ */
			nil,		/* , */
			nil,		/* ] */
			nil,		/* * */
			shift(16),		/* @ */
			
		},

	},
	actionRow{ // S15
				canRecover: false,
		actions: [numSymbols]action{
			nil,		/* INVALID */
			nil,		/* $ */
			nil,		/* # */
			shift(17),		/* id */
			nil,		/* = */
			shift(19),		/* ( */
			nil,		/* | */
			nil,		/* ) */
			shift(20),		/* [ */
			nil,		/* , */
			nil,		/* ] */
			nil,		/* * */
			shift(21),		/* @ */
			
		},

	},
	actionRow{ // S16
				canRecover: false,
		actions: [numSymbols]action{
			nil,		/* INVALID */
			nil,		/* $ */
			nil,		/* # */
			shift(27),		/* id */
			nil,		/* = */
			nil,		/* ( */
			nil,		/* | */
			nil,		/* ) */
			nil,		/* [ */
			nil,		/* , */
			nil,		/* ] */
			nil,		/* * */
			nil,		/* @ */
			
		},

	},
	actionRow{ // S17
				canRecover: false,
		actions: [numSymbols]action{
			nil,		/* INVALID */
			nil,		/* $ */
			nil,		/* # */
			nil,		/* id */
			nil,		/* = */
			nil,		/* ( */
			nil,		/* | */
			nil,		/* ) */
			nil,		/* [ */
			reduce(8),		/* ,, reduce: Expr */
			nil,		/* ] */
			nil,		/* * */
			nil,		/* @ */
			
		},

	},
	actionRow{ // S18
				canRecover: false,
		actions: [numSymbols]action{
			nil,		/* INVALID */
			nil,		/* $ */
			nil,		/* # */
			nil,		/* id */
			nil,		/* = */
			nil,		/* ( */
			nil,		/* | */
			nil,		/* ) */
			nil,		/* [ */
			shift(28),		/* , */
			nil,		/* ] */
			nil,		/* * */
			nil,		/* @ */
			
		},

	},
	actionRow{ // S19
				canRecover: false,
		actions: [numSymbols]action{
			nil,		/* INVALID */
			nil,		/* $ */
			nil,		/* # */
			shift(12),		/* id */
			nil,		/* = */
			shift(14),		/* ( */
			nil,		/* | */
			nil,		/* ) */
			shift(15),		/* [ */
			nil,		/* , */
			nil,		/* ] */
			nil,		/* * */
			shift(16),		/* @ */
			
		},

	},
	actionRow{ // S20
				canRecover: false,
		actions: [numSymbols]action{
			nil,		/* INVALID */
			nil,		/* $ */
			nil,		/* # */
			shift(17),		/* id */
			nil,		/* = */
			shift(19),		/* ( */
			nil,		/* | */
			nil,		/* ) */
			shift(20),		/* [ */
			nil,		/* , */
			nil,		/* ] */
			nil,		/* * */
			shift(21),		/* @ */
			
		},

	},
	actionRow{ // S21
				canRecover: false,
		actions: [numSymbols]action{
			nil,		/* INVALID */
			nil,		/* $ */
			nil,		/* # */
			shift(31),		/* id */
			nil,		/* = */
			nil,		/* ( */
			nil,		/* | */
			nil,		/* ) */
			nil,		/* [ */
			nil,		/* , */
			nil,		/* ] */
			nil,		/* * */
			nil,		/* @ */
			
		},

	},
	actionRow{ // S22
				canRecover: false,
		actions: [numSymbols]action{
			nil,		/* INVALID */
			reduce(7),		/* $, reduce: Expr */
			reduce(7),		/* #, reduce: Expr */
			nil,		/* id */
			nil,		/* = */
			nil,		/* ( */
			nil,		/* | */
			nil,		/* ) */
			nil,		/* [ */
			nil,		/* , */
			nil,		/* ] */
			nil,		/* * */
			nil,		/* @ */
			
		},

	},
	actionRow{ // S23
				canRecover: false,
		actions: [numSymbols]action{
			nil,		/* INVALID */
			nil,		/* $ */
			nil,		/* # */
			shift(32),		/* id */
			nil,		/* = */
			shift(34),		/* ( */
			nil,		/* | */
			nil,		/* ) */
			shift(35),		/* [ */
			nil,		/* , */
			nil,		/* ] */
			nil,		/* * */
			shift(36),		/* @ */
			
		},

	},
	actionRow{ // S24
				canRecover: false,
		actions: [numSymbols]action{
			nil,		/* INVALID */
			nil,		/* $ */
			nil,		/* # */
			nil,		/* id */
			nil,		/* = */
			nil,		/* ( */
			nil,		/* | */
			nil,		/* ) */
			nil,		/* [ */
			nil,		/* , */
			nil,		/* ] */
			shift(37),		/* * */
			nil,		/* @ */
			
		},

	},
	actionRow{ // S25
				canRecover: false,
		actions: [numSymbols]action{
			nil,		/* INVALID */
			nil,		/* $ */
			nil,		/* # */
			nil,		/* id */
			nil,		/* = */
			nil,		/* ( */
			shift(38),		/* | */
			shift(39),		/* ) */
			nil,		/* [ */
			nil,		/* , */
			nil,		/* ] */
			nil,		/* * */
			nil,		/* @ */
			
		},

	},
	actionRow{ // S26
				canRecover: false,
		actions: [numSymbols]action{
			nil,		/* INVALID */
			nil,		/* $ */
			nil,		/* # */
			nil,		/* id */
			nil,		/* = */
			nil,		/* ( */
			nil,		/* | */
			nil,		/* ) */
			nil,		/* [ */
			shift(40),		/* , */
			nil,		/* ] */
			nil,		/* * */
			nil,		/* @ */
			
		},

	},
	actionRow{ // S27
				canRecover: false,
		actions: [numSymbols]action{
			nil,		/* INVALID */
			nil,		/* $ */
			nil,		/* # */
			nil,		/* id */
			nil,		/* = */
			nil,		/* ( */
			reduce(7),		/* |, reduce: Expr */
			reduce(7),		/* ), reduce: Expr */
			nil,		/* [ */
			nil,		/* , */
			nil,		/* ] */
			nil,		/* * */
			nil,		/* @ */
			
		},

	},
	actionRow{ // S28
				canRecover: false,
		actions: [numSymbols]action{
			nil,		/* INVALID */
			nil,		/* $ */
			nil,		/* # */
			shift(41),		/* id */
			nil,		/* = */
			shift(43),		/* ( */
			nil,		/* | */
			nil,		/* ) */
			shift(44),		/* [ */
			nil,		/* , */
			nil,		/* ] */
			nil,		/* * */
			shift(45),		/* @ */
			
		},

	},
	actionRow{ // S29
				canRecover: false,
		actions: [numSymbols]action{
			nil,		/* INVALID */
			nil,		/* $ */
			nil,		/* # */
			nil,		/* id */
			nil,		/* = */
			nil,		/* ( */
			shift(46),		/* | */
			shift(47),		/* ) */
			nil,		/* [ */
			nil,		/* , */
			nil,		/* ] */
			nil,		/* * */
			nil,		/* @ */
			
		},

	},
	actionRow{ // S30
				canRecover: false,
		actions: [numSymbols]action{
			nil,		/* INVALID */
			nil,		/* $ */
			nil,		/* # */
			nil,		/* id */
			nil,		/* = */
			nil,		/* ( */
			nil,		/* | */
			nil,		/* ) */
			nil,		/* [ */
			shift(48),		/* , */
			nil,		/* ] */
			nil,		/* * */
			nil,		/* @ */
			
		},

	},
	actionRow{ // S31
				canRecover: false,
		actions: [numSymbols]action{
			nil,		/* INVALID */
			nil,		/* $ */
			nil,		/* # */
			nil,		/* id */
			nil,		/* = */
			nil,		/* ( */
			nil,		/* | */
			nil,		/* ) */
			nil,		/* [ */
			reduce(7),		/* ,, reduce: Expr */
			nil,		/* ] */
			nil,		/* * */
			nil,		/* @ */
			
		},

	},
	actionRow{ // S32
				canRecover: false,
		actions: [numSymbols]action{
			nil,		/* INVALID */
			nil,		/* $ */
			nil,		/* # */
			nil,		/* id */
			nil,		/* = */
			nil,		/* ( */
			nil,		/* | */
			reduce(8),		/* ), reduce: Expr */
			nil,		/* [ */
			nil,		/* , */
			nil,		/* ] */
			nil,		/* * */
			nil,		/* @ */
			
		},

	},
	actionRow{ // S33
				canRecover: false,
		actions: [numSymbols]action{
			nil,		/* INVALID */
			nil,		/* $ */
			nil,		/* # */
			nil,		/* id */
			nil,		/* = */
			nil,		/* ( */
			nil,		/* | */
			shift(49),		/* ) */
			nil,		/* [ */
			nil,		/* , */
			nil,		/* ] */
			nil,		/* * */
			nil,		/* @ */
			
		},

	},
	actionRow{ // S34
				canRecover: false,
		actions: [numSymbols]action{
			nil,		/* INVALID */
			nil,		/* $ */
			nil,		/* # */
			shift(12),		/* id */
			nil,		/* = */
			shift(14),		/* ( */
			nil,		/* | */
			nil,		/* ) */
			shift(15),		/* [ */
			nil,		/* , */
			nil,		/* ] */
			nil,		/* * */
			shift(16),		/* @ */
			
		},

	},
	actionRow{ // S35
				canRecover: false,
		actions: [numSymbols]action{
			nil,		/* INVALID */
			nil,		/* $ */
			nil,		/* # */
			shift(17),		/* id */
			nil,		/* = */
			shift(19),		/* ( */
			nil,		/* | */
			nil,		/* ) */
			shift(20),		/* [ */
			nil,		/* , */
			nil,		/* ] */
			nil,		/* * */
			shift(21),		/* @ */
			
		},

	},
	actionRow{ // S36
				canRecover: false,
		actions: [numSymbols]action{
			nil,		/* INVALID */
			nil,		/* $ */
			nil,		/* # */
			shift(52),		/* id */
			nil,		/* = */
			nil,		/* ( */
			nil,		/* | */
			nil,		/* ) */
			nil,		/* [ */
			nil,		/* , */
			nil,		/* ] */
			nil,		/* * */
			nil,		/* @ */
			
		},

	},
	actionRow{ // S37
				canRecover: false,
		actions: [numSymbols]action{
			nil,		/* INVALID */
			reduce(6),		/* $, reduce: Expr */
			reduce(6),		/* #, reduce: Expr */
			nil,		/* id */
			nil,		/* = */
			nil,		/* ( */
			nil,		/* | */
			nil,		/* ) */
			nil,		/* [ */
			nil,		/* , */
			nil,		/* ] */
			nil,		/* * */
			nil,		/* @ */
			
		},

	},
	actionRow{ // S38
				canRecover: false,
		actions: [numSymbols]action{
			nil,		/* INVALID */
			nil,		/* $ */
			nil,		/* # */
			shift(32),		/* id */
			nil,		/* = */
			shift(34),		/* ( */
			nil,		/* | */
			nil,		/* ) */
			shift(35),		/* [ */
			nil,		/* , */
			nil,		/* ] */
			nil,		/* * */
			shift(36),		/* @ */
			
		},

	},
	actionRow{ // S39
				canRecover: false,
		actions: [numSymbols]action{
			nil,		/* INVALID */
			nil,		/* $ */
			nil,		/* # */
			nil,		/* id */
			nil,		/* = */
			nil,		/* ( */
			nil,		/* | */
			nil,		/* ) */
			nil,		/* [ */
			nil,		/* , */
			nil,		/* ] */
			shift(54),		/* * */
			nil,		/* @ */
			
		},

	},
	actionRow{ // S40
				canRecover: false,
		actions: [numSymbols]action{
			nil,		/* INVALID */
			nil,		/* $ */
			nil,		/* # */
			shift(41),		/* id */
			nil,		/* = */
			shift(43),		/* ( */
			nil,		/* | */
			nil,		/* ) */
			shift(44),		/* [ */
			nil,		/* , */
			nil,		/* ] */
			nil,		/* * */
			shift(45),		/* @ */
			
		},

	},
	actionRow{ // S41
				canRecover: false,
		actions: [numSymbols]action{
			nil,		/* INVALID */
			nil,		/* $ */
			nil,		/* # */
			nil,		/* id */
			nil,		/* = */
			nil,		/* ( */
			nil,		/* | */
			nil,		/* ) */
			nil,		/* [ */
			nil,		/* , */
			reduce(8),		/* ], reduce: Expr */
			nil,		/* * */
			nil,		/* @ */
			
		},

	},
	actionRow{ // S42
				canRecover: false,
		actions: [numSymbols]action{
			nil,		/* INVALID */
			nil,		/* $ */
			nil,		/* # */
			nil,		/* id */
			nil,		/* = */
			nil,		/* ( */
			nil,		/* | */
			nil,		/* ) */
			nil,		/* [ */
			nil,		/* , */
			shift(56),		/* ] */
			nil,		/* * */
			nil,		/* @ */
			
		},

	},
	actionRow{ // S43
				canRecover: false,
		actions: [numSymbols]action{
			nil,		/* INVALID */
			nil,		/* $ */
			nil,		/* # */
			shift(12),		/* id */
			nil,		/* = */
			shift(14),		/* ( */
			nil,		/* | */
			nil,		/* ) */
			shift(15),		/* [ */
			nil,		/* , */
			nil,		/* ] */
			nil,		/* * */
			shift(16),		/* @ */
			
		},

	},
	actionRow{ // S44
				canRecover: false,
		actions: [numSymbols]action{
			nil,		/* INVALID */
			nil,		/* $ */
			nil,		/* # */
			shift(17),		/* id */
			nil,		/* = */
			shift(19),		/* ( */
			nil,		/* | */
			nil,		/* ) */
			shift(20),		/* [ */
			nil,		/* , */
			nil,		/* ] */
			nil,		/* * */
			shift(21),		/* @ */
			
		},

	},
	actionRow{ // S45
				canRecover: false,
		actions: [numSymbols]action{
			nil,		/* INVALID */
			nil,		/* $ */
			nil,		/* # */
			shift(59),		/* id */
			nil,		/* = */
			nil,		/* ( */
			nil,		/* | */
			nil,		/* ) */
			nil,		/* [ */
			nil,		/* , */
			nil,		/* ] */
			nil,		/* * */
			nil,		/* @ */
			
		},

	},
	actionRow{ // S46
				canRecover: false,
		actions: [numSymbols]action{
			nil,		/* INVALID */
			nil,		/* $ */
			nil,		/* # */
			shift(32),		/* id */
			nil,		/* = */
			shift(34),		/* ( */
			nil,		/* | */
			nil,		/* ) */
			shift(35),		/* [ */
			nil,		/* , */
			nil,		/* ] */
			nil,		/* * */
			shift(36),		/* @ */
			
		},

	},
	actionRow{ // S47
				canRecover: false,
		actions: [numSymbols]action{
			nil,		/* INVALID */
			nil,		/* $ */
			nil,		/* # */
			nil,		/* id */
			nil,		/* = */
			nil,		/* ( */
			nil,		/* | */
			nil,		/* ) */
			nil,		/* [ */
			nil,		/* , */
			nil,		/* ] */
			shift(61),		/* * */
			nil,		/* @ */
			
		},

	},
	actionRow{ // S48
				canRecover: false,
		actions: [numSymbols]action{
			nil,		/* INVALID */
			nil,		/* $ */
			nil,		/* # */
			shift(41),		/* id */
			nil,		/* = */
			shift(43),		/* ( */
			nil,		/* | */
			nil,		/* ) */
			shift(44),		/* [ */
			nil,		/* , */
			nil,		/* ] */
			nil,		/* * */
			shift(45),		/* @ */
			
		},

	},
	actionRow{ // S49
				canRecover: false,
		actions: [numSymbols]action{
			nil,		/* INVALID */
			reduce(4),		/* $, reduce: Expr */
			reduce(4),		/* #, reduce: Expr */
			nil,		/* id */
			nil,		/* = */
			nil,		/* ( */
			nil,		/* | */
			nil,		/* ) */
			nil,		/* [ */
			nil,		/* , */
			nil,		/* ] */
			nil,		/* * */
			nil,		/* @ */
			
		},

	},
	actionRow{ // S50
				canRecover: false,
		actions: [numSymbols]action{
			nil,		/* INVALID */
			nil,		/* $ */
			nil,		/* # */
			nil,		/* id */
			nil,		/* = */
			nil,		/* ( */
			shift(63),		/* | */
			shift(64),		/* ) */
			nil,		/* [ */
			nil,		/* , */
			nil,		/* ] */
			nil,		/* * */
			nil,		/* @ */
			
		},

	},
	actionRow{ // S51
				canRecover: false,
		actions: [numSymbols]action{
			nil,		/* INVALID */
			nil,		/* $ */
			nil,		/* # */
			nil,		/* id */
			nil,		/* = */
			nil,		/* ( */
			nil,		/* | */
			nil,		/* ) */
			nil,		/* [ */
			shift(65),		/* , */
			nil,		/* ] */
			nil,		/* * */
			nil,		/* @ */
			
		},

	},
	actionRow{ // S52
				canRecover: false,
		actions: [numSymbols]action{
			nil,		/* INVALID */
			nil,		/* $ */
			nil,		/* # */
			nil,		/* id */
			nil,		/* = */
			nil,		/* ( */
			nil,		/* | */
			reduce(7),		/* ), reduce: Expr */
			nil,		/* [ */
			nil,		/* , */
			nil,		/* ] */
			nil,		/* * */
			nil,		/* @ */
			
		},

	},
	actionRow{ // S53
				canRecover: false,
		actions: [numSymbols]action{
			nil,		/* INVALID */
			nil,		/* $ */
			nil,		/* # */
			nil,		/* id */
			nil,		/* = */
			nil,		/* ( */
			nil,		/* | */
			shift(66),		/* ) */
			nil,		/* [ */
			nil,		/* , */
			nil,		/* ] */
			nil,		/* * */
			nil,		/* @ */
			
		},

	},
	actionRow{ // S54
				canRecover: false,
		actions: [numSymbols]action{
			nil,		/* INVALID */
			nil,		/* $ */
			nil,		/* # */
			nil,		/* id */
			nil,		/* = */
			nil,		/* ( */
			reduce(6),		/* |, reduce: Expr */
			reduce(6),		/* ), reduce: Expr */
			nil,		/* [ */
			nil,		/* , */
			nil,		/* ] */
			nil,		/* * */
			nil,		/* @ */
			
		},

	},
	actionRow{ // S55
				canRecover: false,
		actions: [numSymbols]action{
			nil,		/* INVALID */
			nil,		/* $ */
			nil,		/* # */
			nil,		/* id */
			nil,		/* = */
			nil,		/* ( */
			nil,		/* | */
			nil,		/* ) */
			nil,		/* [ */
			nil,		/* , */
			shift(67),		/* ] */
			nil,		/* * */
			nil,		/* @ */
			
		},

	},
	actionRow{ // S56
				canRecover: false,
		actions: [numSymbols]action{
			nil,		/* INVALID */
			reduce(5),		/* $, reduce: Expr */
			reduce(5),		/* #, reduce: Expr */
			nil,		/* id */
			nil,		/* = */
			nil,		/* ( */
			nil,		/* | */
			nil,		/* ) */
			nil,		/* [ */
			nil,		/* , */
			nil,		/* ] */
			nil,		/* * */
			nil,		/* @ */
			
		},

	},
	actionRow{ // S57
				canRecover: false,
		actions: [numSymbols]action{
			nil,		/* INVALID */
			nil,		/* $ */
			nil,		/* # */
			nil,		/* id */
			nil,		/* = */
			nil,		/* ( */
			shift(68),		/* | */
			shift(69),		/* ) */
			nil,		/* [ */
			nil,		/* , */
			nil,		/* ] */
			nil,		/* * */
			nil,		/* @ */
			
		},

	},
	actionRow{ // S58
				canRecover: false,
		actions: [numSymbols]action{
			nil,		/* INVALID */
			nil,		/* $ */
			nil,		/* # */
			nil,		/* id */
			nil,		/* = */
			nil,		/* ( */
			nil,		/* | */
			nil,		/* ) */
			nil,		/* [ */
			shift(70),		/* , */
			nil,		/* ] */
			nil,		/* * */
			nil,		/* @ */
			
		},

	},
	actionRow{ // S59
				canRecover: false,
		actions: [numSymbols]action{
			nil,		/* INVALID */
			nil,		/* $ */
			nil,		/* # */
			nil,		/* id */
			nil,		/* = */
			nil,		/* ( */
			nil,		/* | */
			nil,		/* ) */
			nil,		/* [ */
			nil,		/* , */
			reduce(7),		/* ], reduce: Expr */
			nil,		/* * */
			nil,		/* @ */
			
		},

	},
	actionRow{ // S60
				canRecover: false,
		actions: [numSymbols]action{
			nil,		/* INVALID */
			nil,		/* $ */
			nil,		/* # */
			nil,		/* id */
			nil,		/* = */
			nil,		/* ( */
			nil,		/* | */
			shift(71),		/* ) */
			nil,		/* [ */
			nil,		/* , */
			nil,		/* ] */
			nil,		/* * */
			nil,		/* @ */
			
		},

	},
	actionRow{ // S61
				canRecover: false,
		actions: [numSymbols]action{
			nil,		/* INVALID */
			nil,		/* $ */
			nil,		/* # */
			nil,		/* id */
			nil,		/* = */
			nil,		/* ( */
			nil,		/* | */
			nil,		/* ) */
			nil,		/* [ */
			reduce(6),		/* ,, reduce: Expr */
			nil,		/* ] */
			nil,		/* * */
			nil,		/* @ */
			
		},

	},
	actionRow{ // S62
				canRecover: false,
		actions: [numSymbols]action{
			nil,		/* INVALID */
			nil,		/* $ */
			nil,		/* # */
			nil,		/* id */
			nil,		/* = */
			nil,		/* ( */
			nil,		/* | */
			nil,		/* ) */
			nil,		/* [ */
			nil,		/* , */
			shift(72),		/* ] */
			nil,		/* * */
			nil,		/* @ */
			
		},

	},
	actionRow{ // S63
				canRecover: false,
		actions: [numSymbols]action{
			nil,		/* INVALID */
			nil,		/* $ */
			nil,		/* # */
			shift(32),		/* id */
			nil,		/* = */
			shift(34),		/* ( */
			nil,		/* | */
			nil,		/* ) */
			shift(35),		/* [ */
			nil,		/* , */
			nil,		/* ] */
			nil,		/* * */
			shift(36),		/* @ */
			
		},

	},
	actionRow{ // S64
				canRecover: false,
		actions: [numSymbols]action{
			nil,		/* INVALID */
			nil,		/* $ */
			nil,		/* # */
			nil,		/* id */
			nil,		/* = */
			nil,		/* ( */
			nil,		/* | */
			nil,		/* ) */
			nil,		/* [ */
			nil,		/* , */
			nil,		/* ] */
			shift(74),		/* * */
			nil,		/* @ */
			
		},

	},
	actionRow{ // S65
				canRecover: false,
		actions: [numSymbols]action{
			nil,		/* INVALID */
			nil,		/* $ */
			nil,		/* # */
			shift(41),		/* id */
			nil,		/* = */
			shift(43),		/* ( */
			nil,		/* | */
			nil,		/* ) */
			shift(44),		/* [ */
			nil,		/* , */
			nil,		/* ] */
			nil,		/* * */
			shift(45),		/* @ */
			
		},

	},
	actionRow{ // S66
				canRecover: false,
		actions: [numSymbols]action{
			nil,		/* INVALID */
			nil,		/* $ */
			nil,		/* # */
			nil,		/* id */
			nil,		/* = */
			nil,		/* ( */
			reduce(4),		/* |, reduce: Expr */
			reduce(4),		/* ), reduce: Expr */
			nil,		/* [ */
			nil,		/* , */
			nil,		/* ] */
			nil,		/* * */
			nil,		/* @ */
			
		},

	},
	actionRow{ // S67
				canRecover: false,
		actions: [numSymbols]action{
			nil,		/* INVALID */
			nil,		/* $ */
			nil,		/* # */
			nil,		/* id */
			nil,		/* = */
			nil,		/* ( */
			reduce(5),		/* |, reduce: Expr */
			reduce(5),		/* ), reduce: Expr */
			nil,		/* [ */
			nil,		/* , */
			nil,		/* ] */
			nil,		/* * */
			nil,		/* @ */
			
		},

	},
	actionRow{ // S68
				canRecover: false,
		actions: [numSymbols]action{
			nil,		/* INVALID */
			nil,		/* $ */
			nil,		/* # */
			shift(32),		/* id */
			nil,		/* = */
			shift(34),		/* ( */
			nil,		/* | */
			nil,		/* ) */
			shift(35),		/* [ */
			nil,		/* , */
			nil,		/* ] */
			nil,		/* * */
			shift(36),		/* @ */
			
		},

	},
	actionRow{ // S69
				canRecover: false,
		actions: [numSymbols]action{
			nil,		/* INVALID */
			nil,		/* $ */
			nil,		/* # */
			nil,		/* id */
			nil,		/* = */
			nil,		/* ( */
			nil,		/* | */
			nil,		/* ) */
			nil,		/* [ */
			nil,		/* , */
			nil,		/* ] */
			shift(77),		/* * */
			nil,		/* @ */
			
		},

	},
	actionRow{ // S70
				canRecover: false,
		actions: [numSymbols]action{
			nil,		/* INVALID */
			nil,		/* $ */
			nil,		/* # */
			shift(41),		/* id */
			nil,		/* = */
			shift(43),		/* ( */
			nil,		/* | */
			nil,		/* ) */
			shift(44),		/* [ */
			nil,		/* , */
			nil,		/* ] */
			nil,		/* * */
			shift(45),		/* @ */
			
		},

	},
	actionRow{ // S71
				canRecover: false,
		actions: [numSymbols]action{
			nil,		/* INVALID */
			nil,		/* $ */
			nil,		/* # */
			nil,		/* id */
			nil,		/* = */
			nil,		/* ( */
			nil,		/* | */
			nil,		/* ) */
			nil,		/* [ */
			reduce(4),		/* ,, reduce: Expr */
			nil,		/* ] */
			nil,		/* * */
			nil,		/* @ */
			
		},

	},
	actionRow{ // S72
				canRecover: false,
		actions: [numSymbols]action{
			nil,		/* INVALID */
			nil,		/* $ */
			nil,		/* # */
			nil,		/* id */
			nil,		/* = */
			nil,		/* ( */
			nil,		/* | */
			nil,		/* ) */
			nil,		/* [ */
			reduce(5),		/* ,, reduce: Expr */
			nil,		/* ] */
			nil,		/* * */
			nil,		/* @ */
			
		},

	},
	actionRow{ // S73
				canRecover: false,
		actions: [numSymbols]action{
			nil,		/* INVALID */
			nil,		/* $ */
			nil,		/* # */
			nil,		/* id */
			nil,		/* = */
			nil,		/* ( */
			nil,		/* | */
			shift(79),		/* ) */
			nil,		/* [ */
			nil,		/* , */
			nil,		/* ] */
			nil,		/* * */
			nil,		/* @ */
			
		},

	},
	actionRow{ // S74
				canRecover: false,
		actions: [numSymbols]action{
			nil,		/* INVALID */
			nil,		/* $ */
			nil,		/* # */
			nil,		/* id */
			nil,		/* = */
			nil,		/* ( */
			nil,		/* | */
			reduce(6),		/* ), reduce: Expr */
			nil,		/* [ */
			nil,		/* , */
			nil,		/* ] */
			nil,		/* * */
			nil,		/* @ */
			
		},

	},
	actionRow{ // S75
				canRecover: false,
		actions: [numSymbols]action{
			nil,		/* INVALID */
			nil,		/* $ */
			nil,		/* # */
			nil,		/* id */
			nil,		/* = */
			nil,		/* ( */
			nil,		/* | */
			nil,		/* ) */
			nil,		/* [ */
			nil,		/* , */
			shift(80),		/* ] */
			nil,		/* * */
			nil,		/* @ */
			
		},

	},
	actionRow{ // S76
				canRecover: false,
		actions: [numSymbols]action{
			nil,		/* INVALID */
			nil,		/* $ */
			nil,		/* # */
			nil,		/* id */
			nil,		/* = */
			nil,		/* ( */
			nil,		/* | */
			shift(81),		/* ) */
			nil,		/* [ */
			nil,		/* , */
			nil,		/* ] */
			nil,		/* * */
			nil,		/* @ */
			
		},

	},
	actionRow{ // S77
				canRecover: false,
		actions: [numSymbols]action{
			nil,		/* INVALID */
			nil,		/* $ */
			nil,		/* # */
			nil,		/* id */
			nil,		/* = */
			nil,		/* ( */
			nil,		/* | */
			nil,		/* ) */
			nil,		/* [ */
			nil,		/* , */
			reduce(6),		/* ], reduce: Expr */
			nil,		/* * */
			nil,		/* @ */
			
		},

	},
	actionRow{ // S78
				canRecover: false,
		actions: [numSymbols]action{
			nil,		/* INVALID */
			nil,		/* $ */
			nil,		/* # */
			nil,		/* id */
			nil,		/* = */
			nil,		/* ( */
			nil,		/* | */
			nil,		/* ) */
			nil,		/* [ */
			nil,		/* , */
			shift(82),		/* ] */
			nil,		/* * */
			nil,		/* @ */
			
		},

	},
	actionRow{ // S79
				canRecover: false,
		actions: [numSymbols]action{
			nil,		/* INVALID */
			nil,		/* $ */
			nil,		/* # */
			nil,		/* id */
			nil,		/* = */
			nil,		/* ( */
			nil,		/* | */
			reduce(4),		/* ), reduce: Expr */
			nil,		/* [ */
			nil,		/* , */
			nil,		/* ] */
			nil,		/* * */
			nil,		/* @ */
			
		},

	},
	actionRow{ // S80
				canRecover: false,
		actions: [numSymbols]action{
			nil,		/* INVALID */
			nil,		/* $ */
			nil,		/* # */
			nil,		/* id */
			nil,		/* = */
			nil,		/* ( */
			nil,		/* | */
			reduce(5),		/* ), reduce: Expr */
			nil,		/* [ */
			nil,		/* , */
			nil,		/* ] */
			nil,		/* * */
			nil,		/* @ */
			
		},

	},
	actionRow{ // S81
				canRecover: false,
		actions: [numSymbols]action{
			nil,		/* INVALID */
			nil,		/* $ */
			nil,		/* # */
			nil,		/* id */
			nil,		/* = */
			nil,		/* ( */
			nil,		/* | */
			nil,		/* ) */
			nil,		/* [ */
			nil,		/* , */
			reduce(4),		/* ], reduce: Expr */
			nil,		/* * */
			nil,		/* @ */
			
		},

	},
	actionRow{ // S82
				canRecover: false,
		actions: [numSymbols]action{
			nil,		/* INVALID */
			nil,		/* $ */
			nil,		/* # */
			nil,		/* id */
			nil,		/* = */
			nil,		/* ( */
			nil,		/* | */
			nil,		/* ) */
			nil,		/* [ */
			nil,		/* , */
			reduce(5),		/* ], reduce: Expr */
			nil,		/* * */
			nil,		/* @ */
			
		},

	},
	
}
