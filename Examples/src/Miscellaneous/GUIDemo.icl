implementation module GUIDemo

import iTasks
import CommonDomain

guiDemoExample :: [Workflow]
guiDemoExample
	= [ workflow "Examples/Miscellaneous/GUI Demo" guiDemo]

:: Address =
	{ street		:: String
	, number		:: Int
	, postalCode	:: String
	}

:: Person =
	{ name			:: String
	, cool			:: Bool
	, dob			:: Date
	, tob			:: Time
	, age			:: Maybe Int
	, address		:: Address
	, grades		:: [Int]
	, note			:: Maybe Note
	}

derive gPrint		Person, Address
derive gParse		Person, Address
derive gVisualize	Person, Address
derive gUpdate		Person, Address

address = {Address | street = "Heyendaalseweg", number = 135, postalCode = "6525 AJ"}
person	= {Person | name	= "John Doe"
				  , cool	= True
				  , dob		= {year = 1978, mon = 4, day = 1}
				  , tob		= {hour = 13, min = 42, sec = 0}
				  , age		= Just 23
				  , address	= address
				  , grades	= []
				  , note	= Nothing
				  }
guiDemo :: Task Void
guiDemo
	=	updateInformation "You may change this information" person
	>>=	showMessageAbout "This is the information you entered"