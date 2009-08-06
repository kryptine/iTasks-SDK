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
	, age			:: Maybe Int
	, address		:: Address
	, grades		:: [Int]
	}

derive gPrint		Person, Address
derive gParse		Person, Address
derive gVisualize	Person, Address
derive gUpdate		Person, Address

myPerson = {name = "Bas", cool = True, address = myAddress, age = Just 25, grades = []}
myAddress = {street = "Berg en Dalseweg", number = 24, postalCode = "6521JG"}

Start :: *World -> *World
Start world = startEngine [workflow "GUI test" guiTestTask] world

guiTestTask :: Task Person
guiTestTask = requestInformationWD "Please tell me who you are" myPerson