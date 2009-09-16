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

myPerson = {name = "Bas", cool = True, dob = {Date | year = 1984, mon = 1, day = 13}, tob = {Time| hour= 8, min = 23, sec = 0}, address = myAddress, age = Just 25, grades = [7,8,6,8], note = Note ""}
myAddress = {street = "Berg en Dalseweg", number = 24, postalCode = "6521JG"}

Start :: *World -> *World
Start world = startEngine [workflow "GUI test" guiTestTask] world

guiTestTask :: Task Void
guiTestTask = updateInformation "Please update your personal information" myPerson  >>= showMessageAbout "You have entered:"
//guiTestTask = enterMultipleChoice "Pick your fruits" ["Apple","Pear","Orange"] >>= showMessageAbout "Your selection:" 
//guiTestTask = enterMultipleChoice "Pick your numbers" [0..30] >>= showMessageAbout "Your selection:" 
//guiTestTask = updateChoice "Pick your favorite fruit" ["Apple","Pear","Orange"] 1 >>= showMessageAbout "Your choice:" 