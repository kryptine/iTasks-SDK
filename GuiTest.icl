module GuiTest

import iTasks

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

myAddress = {street = "Berg en Dalseweg", number = 24, postalCode = "6521JG"}
myPerson = {name = "Bas", cool = True, address = myAddress, age = Just 25, grades = []}

myDefault :: Person
myDefault = defaultValue

derive gVisualize Person, Address
derive gUpdate Person, Address

derive gPrint Person, Address
derive gParse Person, Address

//TO BE OBSOLETE
derive  gForm	Person, Address
derive	gUpd	Person, Address

Start :: *World -> *World
Start world = startEngine [workflow "GUI test" guiTestTask] world

guiTestTask :: Task Person
guiTestTask = requestInformationWD "Please tell me who you are" myPerson