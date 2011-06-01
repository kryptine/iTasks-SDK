implementation module Chapter5

// Examples showing the extension of editors with buttons

import iTasks
from Chapter2 import show
from Chapter3 import positive

derive bimap (,), Maybe

Start :: *World -> *World
Start world = startEngine flows5 world

flows5 :: [Workflow]
flows5 =  [w1,w2,w3,w4,w5]

w1 = workflow "CEFP/Chap 5/1. Dynamic number of buttons" "Dynamic number of buttons to choose from" (forever (show (positive >>= actions)))
w2 = workflow "CEFP/Chap 5/2. Dynamic number of buttons" "Order pressed is remembered" (show (dynButtons [1..10] []))
w3 = workflow "CEFP/Chap 5/3. Accept only an odd number" "Type in an odd positive number less than 100" (show getOddNumber)
w4 = workflow "CEFP/Chap 5/4. Odd or Even" "Either the odd or even buttons can be chosen" (show (oddOrEvenButtons True))
w5 = workflow "CEFP/Chap 5/5. Palindrome exercise" "Palindrome" palindrome

// create n buttons

actions :: Int -> Task Int
actions n = chooseAction
            [  (Action ("action" +++ toString i) ("label " +++ toString i),i)
            \\ i <- [1..n]
            ]

// n buttons, order pressed is remembered

dynButtons :: [Int] [Int] -> Task [Int]
dynButtons [] accu = return accu
dynButtons numbers accu
	=					updateInformation "Choose a button" [] Void
		>?* 			[(Action (toString i) (toString i), Always (return index)) \\ i <- numbers & index <- [0..]] 
		>>= \index ->	dynButtons (removeAt index numbers) [numbers!!index:accu]

// accept only an odd number 

getOddNumber :: Task Int
getOddNumber 
	=			enterInformation "Type in an odd number" [] 
		>?*  	[(ActionOk, Sometimes (onlyIf (\n -> n > 0 && isOdd n && n < 100) return))]
	
onlyIf :: (a -> Bool) (a -> Task b) (InformationState a) -> Maybe (Task b)
onlyIf pred taskf  s
| not 	s.localValid  = Nothing
| pred  s.modelValue  = Just (taskf s.modelValue)
= Nothing

// show only the even or odd buttons

oddOrEvenButtons :: Bool ->Task Int
oddOrEvenButtons even
	=			updateInformation "Choose a button" [] Void
		>?*		[ (Action "Odd" "Odd",  				Sometimes (onlyIf (\_ -> even)     (\_ -> oddOrEvenButtons (not even))))
				, (Action "Even" "Even",				Sometimes (onlyIf (\_ -> not even) (\_ -> oddOrEvenButtons (not even))))
				: [ (Action (toString i) (toString i),  Always (return i))
				  \\ i <- [0..9] | if even (isEven i) (isOdd i)
				  ]
				] 
	
// palindrome	

palindrome 
	= 				enterInformation "Please enter a text" []
		>?*	 		[(Action "palin"   "Palindrome!",Sometimes (onlyIf isPalindrome return))
					,(Action "nopalin" "Nope!",      Sometimes (onlyIf (not o isPalindrome) return))
					]
where	
	isPalindrome :: String -> Bool
	isPalindrome str	= chars == reverse chars
	where
		chars :: [Char]
		chars			= fromString str
	