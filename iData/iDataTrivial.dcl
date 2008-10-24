definition module iDataTrivial

import StdMaybe, StdGeneric, StdOverloaded

// utility 

mkString		:: ![Char] -> *String
mkList			:: !String -> [Char]
stl				:: !u:[.a] -> v:[.a], [u <= v]

//	Useful string concatenation function
(<+++) infixl	:: !String !a -> String | toString a
(+++>) infixr	:: !a !String -> String | toString a

(??) infixl 9	:: ![a] !a -> Int | == a

const2			:: .a !.b -> .b
