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

w1 = workflow "CEFP/Chap 5/1. Choose actions" "Choose actions" (forever (show (positive >>= actions)))
w2 = workflow "CEFP/Chap 5/2. Palindrome exercise" "Palindrome" palindrome

actions :: Int -> Task Int
actions n = chooseAction
            [  (Action ("action" +++ toString i) ("label " +++ toString i),i)
            \\ i <- [1..n]
            ]

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
