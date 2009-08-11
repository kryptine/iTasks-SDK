module GuiTest

import iTasks
import CommonDomain

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
	, note			:: Note
	}

derive gPrint		Person, Address
derive gParse		Person, Address
derive gVisualize	Person, Address
derive gUpdate		Person, Address

myPerson = {name = "Bas", cool = True, dob = {Date | year = 1984, mon = 1, day = 13}, tob = {Time| hour= 8, min = 23, sec = 0}, address = myAddress, age = Just 25, grades = [], note = Note ""}
myAddress = {street = "Berg en Dalseweg", number = 24, postalCode = "6521JG"}

Start :: *World -> *World
Start world = startEngine [workflow "GUI test" guiTestTask] world

guiTestTask :: Task Person
guiTestTask = requestInformation "Please enter your personal information"