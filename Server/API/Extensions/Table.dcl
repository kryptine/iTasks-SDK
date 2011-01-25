definition module Table

import GenVisualize

derive gVisualize	Table
derive gUpdate		Table
derive gVerify		Table
derive JSONEncode	Table
derive JSONDecode	Table

derive gMakeColumns	OBJECT, CONS, FIELD, PAIR, Int, String, Bool, Real, Date, Currency
derive gMakeRow		OBJECT, CONS, FIELD, PAIR, Int, String, Bool, Real, Date, Currency

:: Table = Table ![TUIGridColumn] ![JSONNode]

toTable :: ![a] -> Table | tableRow a

class tableRow a
	| gMakeColumns{|*|}
	, gMakeRow{|*|} a

generic gMakeColumns	a :: !a ![String] !(Maybe Int)	!DataPath -> (![TUIGridColumn],!DataPath)
generic gMakeRow		a :: !a							!DataPath -> (![(!String,!String)],!DataPath)
