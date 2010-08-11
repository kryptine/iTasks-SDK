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
	, city			:: String
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

derive class iTask	Person, Address
derive bimap (,), Maybe

address = {Address | street = "Heyendaalseweg", number = 135, postalCode = "6525 AJ", city = "Nijmegen"}
person	= {Person | name	= "John Doe"
				  , cool	= True
				  , dob		= {Date | year = 1978, mon = 4, day = 1}
				  , tob		= {Time | hour = 13, min = 42, sec = 0}
				  , age		= Just 23
				  , address	= address
				  , grades	= []
				  , note	= Nothing
				  }
guiDemo :: Task Void
guiDemo
	=	updateInformation "You may change this information" person
	>>=	showMessageAbout "This is the information you entered"