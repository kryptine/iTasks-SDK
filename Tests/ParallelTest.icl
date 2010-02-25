module ParallelTest

import iTasks

enterFoo:: Task Int
enterFoo = "42" @>>
	enterInformation "Value 1"

test1 :: Task (Int,Int)
test1 = (enterFoo -&&- enterInformation "Value 2")


test2 :: Task ([Int],Int)
test2 = enterInformation "Value 1" -&&-
	("Val 2" @>>( enterInformation "Value 2" >>= \val ->
		(updateInformation "Value 2.1" val -||- updateInformation "Value 2.2" val)))

Start :: *World -> *World
Start world = startEngine [
			workflow "Normal"	  (enterFoo >>= showMessageAbout "Result"),
			workflow "Par Test 1" (test1  >>= showMessageAbout "Result"),
			workflow "Par Test 2" (test2  >>= showMessageAbout "Result")
		] world 