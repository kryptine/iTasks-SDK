implementation module Chapter5

// Examples showing the extension of editors with buttons

import iTasks
from Chapter2 import show
from Chapter3 import positive

derive bimap (,), Maybe

Start :: *World -> *World
Start world = startEngine flows5 world

flows5 :: [Workflow]
flows5 =  [w0, w1,w2,w3,w4,w5]

w0 = workflow "CEFP/Chap 5/0. Simple Question" 				"Only one answer possible..." 					(show ask)
w1 = workflow "CEFP/Chap 3/1. Form for [Person]" 			"Form for [Person]" 							(show personList6)
w2 = workflow "CEFP/Chap 5/2. Accept only an even number" 	"Type in an even number" 						(show askEven)
w3 = workflow "CEFP/Chap 5/3. Only even" 					"Either the odd or even buttons can be chosen" 	(show (oddOrEvenButtons True))
w4 = workflow "CEFP/Chap 5/4. Dynamic number of buttons" 	"Dynamic number of buttons to choose from" 		(forever (show (positive >>= actions)))
w5 = workflow "CEFP/Chap 5/5. Dynamic number of buttons" 	"Order pressed is remembered" 					(show (dynButtons [1..10] []))
w6 = workflow "CEFP/Chap 5/6. Palindrome exercise" 			"Palindrome" 									palindrome

// simple question with buttons

ask :: Task Bool
ask
	=		showInformation "Do you like the iTask system ?" [] Void
		>?* [(ActionYes, Always (showInformation "Thank you !" [] True))
			,(ActionNo,  Always (showInformation "Perhaps you did not onderstand the question" [] False >>| ask))
			]

//

:: Person 	= 	{ firstName    	:: String
		      	, surName  		:: String
		      	, dateOfBirth 	:: Date
		      	, gender	 	:: Gender
		      	}
:: Gender 	=	Male
			|	Female

derive class iTask Person, Gender

personList6 :: Task [Person]
personList6
	=         enterInformation "Please fill in the form" []
		>?*  [(Action "Add one ", IfValid morePersons)  
			 ,(Action "Quit", IfValid (\person -> return [person]))
			 ]
where
		morePersons person
			=					personList6
				>>= \persons -> return [person:persons]


// accept only an even number 

askEven :: Task Int
askEven
	=		enterInformation "Please type in an even number..." [] 
		>?* [(ActionOk, Sometimes (onlyIf isEven return))
			]

onlyIf :: (a -> Bool) (a -> Task b) (InformationState a) -> Maybe (Task b)
onlyIf pred taskf  s
| not 	s.localValid  = Nothing
| pred  s.modelValue  = Just (taskf s.modelValue)
= Nothing


// create n buttons

actions :: Int -> Task Int
actions n = chooseAction
            [  (Action ("label " +++ toString i),i)
            \\ i <- [1..n]
            ]

// n buttons, order pressed is remembered

dynButtons :: [Int] [Int] -> Task [Int]
dynButtons [] accu = return accu
dynButtons numbers accu
	=					updateInformation "Choose a button" [] Void
		>?* 			[(Action (toString i), Always (return index)) \\ i <- numbers & index <- [0..]] 
		>>= \index ->	dynButtons (removeAt index numbers) [numbers!!index:accu]

	

// show only the even or odd buttons

oddOrEvenButtons :: Bool ->Task Int
oddOrEvenButtons even
	=			updateInformation "Choose a button" [] Void
		>?*		[ (Action "Odd",  				Sometimes (onlyIf (\_ -> even)     (\_ -> oddOrEvenButtons (not even))))
				, (Action "Even",				Sometimes (onlyIf (\_ -> not even) (\_ -> oddOrEvenButtons (not even))))
				: [ (Action (toString i),  Always (return i))
				  \\ i <- [0..9] | if even (isEven i) (isOdd i)
				  ]
				] 
	
// palindrome	

palindrome 
	= 				enterInformation "Please enter a text" []
		>?*	 		[(Action "Palindrome!",Sometimes (onlyIf isPalindrome return))
					,(Action "Nope!",      Sometimes (onlyIf (not o isPalindrome) return))
					]
where	
	isPalindrome :: String -> Bool
	isPalindrome str	= chars == reverse chars
	where
		chars :: [Char]
		chars			= fromString str
	