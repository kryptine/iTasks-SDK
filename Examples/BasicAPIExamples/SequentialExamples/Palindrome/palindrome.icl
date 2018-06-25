module palindrome

// Enter a palindrome 

import iTasks

Start :: *World -> *World
Start world 
	= startEngine palindrome world
	
palindrome :: Task (Maybe String)
palindrome 
	=   	enterInformation "Enter a palindrome" []
	>>* 	[ OnAction  ActionOk     (ifValue palindrome (\v -> return (Just v)))
            , OnAction  ActionCancel (always (return Nothing))
            ]
    >>=		viewInformation "Result is:" []
    >>=		return
where
	palindrome s = lc == reverse lc
	where lc :: [Char]
		  lc = fromString s
