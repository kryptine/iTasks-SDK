definition module Table

import GenVisualize

derive gVisualize	Table
derive gUpdate		Table
derive gDefaultMask	Table
derive gVerify		Table
derive JSONEncode	Table
derive JSONDecode	Table
derive gEq			Table

derive gMakeColumns	OBJECT, CONS, FIELD, PAIR, Int, String, Bool, Real, Date, Note
derive gToRow		OBJECT, CONS, FIELD, PAIR, Int, String, Bool, Real, Date, Note
derive gFromRow		OBJECT, CONS, FIELD, PAIR, Int, String, Bool, Real, Date, Note
derive bimap		TableCol

:: Table a = Table ![TableCol a] ![JSONNode]
:: TableCol a =
	{ header		:: !String
	, dataIndex		:: !String
	, editorType	:: !Maybe String
	}

toTable		:: ![a] -> (Table a) | tableRow a
fromTable	:: !(Table a) -> [a] | tableRow a

class tableRow a
	| gMakeColumns{|*|}
	, gToRow{|*|}
	, gFromRow{|*|} a

generic gMakeColumns	a :: ![String] !(Maybe Int)	!DataPath -> (![TableCol a],!DataPath)
generic gToRow			a :: !a						!DataPath -> (![(!String,!String)],!DataPath)
generic gFromRow		a :: !(Map String String)	!DataPath -> (!a,!DataPath)
