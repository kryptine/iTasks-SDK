implementation module Chapter5

// Examples showing the extension of editors with buttons

import iTasks
from Chapter2 import show
from Chapter3 import positive

derive bimap (,), Maybe

Start :: *World -> *World
Start world = startEngine flows5 world

flows5 :: [Workflow]
flows5 =  [w1,w2]

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
	=					enterInformationA "Choose a button" action
		>>= \index ->	dynButtons (removeAt index numbers) [numbers!!index:accu]
where
	action :: (Maybe Void) -> [(Action,Maybe Int)]
	action n = [(Action (toString i) (toString i), Just index) \\ i <- numbers & index <- [0..]] 

// accept only an odd number 

getOddNumber :: Task Int
getOddNumber 
	=					enterInformationA "Type in an odd number" action 
where
	action n	= [(ActionOk, onlyIf (\n -> (n > 0 && isOdd n && n < 100)) n)]
	
onlyIf :: (a -> Bool) (Maybe a) -> Maybe a
onlyIf pred (Just a)
	| pred a = Just a
onlyIf _ _ = Nothing

// show only the even or odd buttons

oddOrEvenButtons :: Bool ->Task Int
oddOrEvenButtons even
	=					enterInformationA "Choose a button" action
		>>= \mbInt ->	if (isNothing mbInt) (oddOrEvenButtons (not even)) (return (fromJust mbInt))
where
	action :: (Maybe Void) -> [(Action,Maybe (Maybe Int))]
	action n =  [ (Action "Odd" "Odd",  if even 	  (Just Nothing) Nothing)
				, (Action "Even" "Even",if (not even) (Just Nothing) Nothing)
				: [ (Action (toString i) (toString i), if even (onOff isEven i) (onOff isOdd i))
				  \\ i <- [0..9] 
				  ]
				] 
	onOff pred i = if (pred i) (Just (Just i)) Nothing
	
// palindrome	

palindrome = enterInformationA "Please enter a text" actionf
where
	actionf Nothing = [(Action "none" "No text entered", Nothing)]
	actionf (Just x)= [(Action "palin"   "Palindrome!",if      (isPalindrome x)  (Just x) Nothing)
					  ,(Action "nopalin" "Nope!",      if (not (isPalindrome x)) (Just x) Nothing)
					  ]
	
	isPalindrome :: String -> Bool
	isPalindrome str	= chars == reverse chars
	where
		chars :: [Char]
		chars			= fromString str
	