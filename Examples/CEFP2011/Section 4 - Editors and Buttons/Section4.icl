implementation module Section4

// Examples showing the extension of editors with buttons

import iTasks
from Section3 import show, positive

derive bimap (,), Maybe

Start :: *World -> *World
Start world = startEngine flows4 world

flows4 :: [Workflow]
flows4 =  [w1, w2, w3, w4, w5, w6, w7, w8]

w1	= workflow "CEFP/Sect 4/1. Absolutely 42"				"Yield 42 or absolute value"					absolute
w2	= workflow "CEFP/Sect 4/2. Simple Question" 			"Only one answer possible..." 					(show ask)
w3	= workflow "CEFP/Sect 4/3. Form for [Person]" 			"Form for [Person]" 							(show personList6)
w4	= workflow "CEFP/Sect 4/4. Accept only an even number" 	"Type in an even number" 						(show askEven)
w5	= workflow "CEFP/Sect 4/5. Only even" 					"Either the odd or even buttons can be chosen" 	(show (oddOrEvenButtons True))
w6	= workflow "CEFP/Sect 4/6. Dynamic number of buttons" 	"Dynamic number of buttons to choose from" 		(forever (show (positive >>= actions)))
w7	= workflow "CEFP/Sect 4/7. Dynamic number of buttons" 	"Order pressed is remembered" 					(show (dynButtons [1..10] []))
w8	= workflow "CEFP/Sect 4/8. Palindrome exercise" 		"Palindrome" 									palindrome

// return 42 or absolute value of input
absolute :: Task Int
absolute = enterInformation "Enter a number" []
     >?* [(Action "Always",    Always   (return 42))
         ,(Action "If valid",  IfValid  (\x -> return (abs x)))
         ,(Action "Sometimes", Sometimes check_positive)
         ]
where
	check_positive :: (InformationState Int) -> Maybe (Task Int)
	check_positive {localValid,modelValue}
	| localValid && modelValue > 0	= Just (return modelValue)
	| otherwise						= Nothing

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
			 ,(Action "Quit",     IfValid (\person -> return [person]))
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

palindrome :: Task (Maybe String)
palindrome 
	= 				enterInformation "Please enter a text" []
		>?*	 		[(ActionOk,   Sometimes (onlyIf isPalindrome (return o Just)))
					,(ActionQuit, Always (return Nothing))
					]
where	
	isPalindrome :: String -> Bool
	isPalindrome str	= chars == reverse chars
	where
		chars :: [Char]
		chars			= fromString str
	