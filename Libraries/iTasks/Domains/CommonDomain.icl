implementation module CommonDomain

import iTasks
import GenPrint, GenParse, GenLexOrd, GUICore

derive gPrint		EmailAddress, Password, Note, Date, Time, Money, Currency
derive gParse		EmailAddress, Password, Note, Date, Time, Money, Currency
derive gVisualize	EmailAddress, Password, Note, Date, Time, Money, Currency
derive gUpdate		EmailAddress, Password, Note, Date, Time, Money, Currency
derive gLexOrd		Money, Currency

instance toString Money
where
	toString x = visualizeAsTextLabel x
	 
instance toString Currency
where
	toString x = visualizeAsTextLabel x

instance toInt Money
where
	toInt (Money _ val) = val
		
instance < Money
where
	(<) x y = case x =?= y of
		LT	= True
		_	= False
		
instance zero Money
where
	zero = Money EUR 0

instance + Money
where
	(+) (Money curx valx) (Money cury valy) = (Money curx (valx + valy)) //Never add different currencies!