implementation module JSON

import StdEnv
import StdGeneric, StdMaybe
import Text

toJSON :: a -> String | JSONEncode{|*|} a
toJSON x = join "" (JSONEncode{|*|} x [])

//fromJSON :: String -> Maybe a | JSONDecode{|*|} a
 

/*
* Generic JSON printer, using a list of strings as continuation
*/
generic JSONEncode t :: t [String] -> [String]

JSONEncode{|Int|} x c = [toString x:c]
JSONEncode{|Char|} x c = [toString x:c]
JSONEncode{|Bool|} True c = ["true":c]
JSONEncode{|Bool|} False c = ["false":c]
JSONEncode{|String|} x c = ["\"",escape x,"\"":c]
JSONEncode{|UNIT|} (UNIT) c = c
JSONEncode{|PAIR|} fx fy (PAIR x y) c = fx x [", " : fy y c]
JSONEncode{|EITHER|} fx fy (LEFT x) c = fx x c
JSONEncode{|EITHER|} fx fy (RIGHT y) c = fy y c
JSONEncode{|OBJECT|} fx (OBJECT x) c = fx x c
JSONEncode{|CONS of d|} fx (CONS x) c
	| length d.gcd_fields <> 0	= ["{": fx x ["}":c]] 						//Record
	| d.gcd_arity == 0			= ["\"",d.gcd_name,"\"":c]					//Constructor without parameters
								= ["[\"",d.gcd_name,"\", ": fx x ["]":c]]	//Constructor with parameters

	
JSONEncode{|FIELD of d|} fx (FIELD x) c = [d.gfd_name, ": " : fx x c]							
JSONEncode{|[]|} fx x c = JSONEncodeList fx x c
JSONEncode{|{}|} fx x c = JSONEncodeList fx [e \\ e <-: x] c
JSONEncode{|{!}|} fx x c = JSONEncodeList fx [e \\ e <-: x] c

//List generation for lists and arrays
JSONEncodeList fx x c = ["[": ( flatten ( intersperse [","] (map (flip fx []) x)) ) ++ ["]": c]]

//Escape a string
escape :: String -> String
escape s = replaceSubString "\"" "\\\"" (replaceSubString "\\" "\\\\" s)

//Intersperse an element on a list
intersperse :: a [a] -> [a]
intersperse i [] = []
intersperse i [x] = [x]
intersperse i [x:xs] = [x,i:intersperse i xs]

/**
* Generic JSON parser.
*/
//generic JSONDecode t :: [Token] -> Maybe t












