implementation module EncodeDecode

// encoding and decoding of information
// (c) 2005 MJP

import StdEnv, StdMaybe, ArgEnv, Directory
import iDataTrivial
import GenPrint, GenParse, GenBimap
import dynamic_string
import Http, HttpTextUtil
import FormId

// low level url encoding decoding of Strings

encodeString :: !String -> String
encodeString s							= string_to_string52 s

decodeString :: !String -> *String
decodeString s							= string52_to_string s


// utility functions based on low level encoding - decoding

encodeInfo :: !a -> String | gPrint{|*|} a
encodeInfo inp							= encodeString (printToString inp)

decodeInfo :: !String -> Maybe a | gParse{|*|} a
decodeInfo str							= parseString (decodeString str)

decodeChars :: ![Char] -> *String
decodeChars cs							= decodeString (mkString cs)

// compact John van Groningen encoding-decoding to lower and uppercase alphabet

string_to_string52 :: !String -> *String
string_to_string52 s
# n		=	size s
# n3d2	=	3*(n>>1)
| n bitand 1==0
= fill_string52 0 0 n s (createArray n3d2 '\0')
# a = fill_string52 0 0 (n-1) s (createArray (n3d2+2) '\0')
  i=toInt s.[n-1]
  i1=i/52
  r0=i-i1*52
= {a & [n3d2]=int52_to_alpha_char i1,[n3d2+1]=int52_to_alpha_char r0} 
where
	fill_string52 :: !Int !Int !Int !String !*String -> *String
	fill_string52 si ai l s a
	| si<l
	# i=toInt s.[si]<<8+toInt s.[si+1]
	  i1=i/52
	  i2=i1/52
	  r0=i-i1*52
	  r1=i1-i2*52
	  a={a & [ai]=int52_to_alpha_char i2,[ai+1]=int52_to_alpha_char r1,[ai+2]=int52_to_alpha_char r0}
	= fill_string52 (si+2) (ai+3) l s a
	= a

int52_to_alpha_char i :== toChar (i-(((i-26)>>8) bitand 6)+71)

string52_to_string :: !String -> *String
string52_to_string s
# n		=	size s
# nd3	=	n/3
# r3	=	n-nd3*3
# n2d3	=	nd3<<1
| r3==0	= fill_string 0 0 n s (createArray n2d3 '\0')
| r3==2
# a = fill_string 0 0 (n-2) s (createArray (n2d3+1) '\0')
= {a & [n2d3]=toChar (alpha_to_int52 s.[n-2]*52+alpha_to_int52 s.[n-1])}
// The following actually should *never* happen, but sometimes it does because the browser returns ill-values!
// To prevent the application from crashing, a default case has been added
| otherwise = ""
where
	fill_string :: !Int !Int !Int !String !*String -> *String
	fill_string si ai l s a
	| si<l
	# i=(alpha_to_int52 s.[si]*52+alpha_to_int52 s.[si+1])*52+alpha_to_int52 s.[si+2]
	# a={a & [ai]=toChar (i>>8),[ai+1]=toChar i}
	= fill_string (si+3) (ai+2) l s a
	= a

alpha_to_int52 c
:== let i=toInt c in i+(((i-97)>>8) bitand 6)-71

// small parsing utility functions

mscan :: Char ![Char] -> ([Char],[Char])
mscan c list							= case span ((<>) c) list of			// scan like span but it removes character
											(x,[])	= (x,[])
											(x,y)	= (x,tl y)

skipping :: !.[a] !u:[a] -> v:[a] | == a, [u <= v]
skipping [c:cs] list=:[x:xs]
| c == x								= skipping cs xs
| otherwise								= list
skipping any    list					= list

// The following code is not used, but is included as reference code and for debugging purposes.

// encoding - decoding to hexadecimal code

urlEncode :: !String -> String
urlEncode s								= mkString (urlEncode` (mkList s))
where
	urlEncode` :: ![Char] -> [Char]
	urlEncode` []						= []
	urlEncode` [x:xs] 
	| isAlphanum x						= [x  : urlEncode` xs]
	| otherwise							= urlEncodeChar x ++ urlEncode` xs
	where
		urlEncodeChar x 
		# (c1,c2)						= charToHex x
		= ['%', c1 ,c2]
	
		charToHex :: !Char -> (!Char, !Char)
		charToHex c						= (toChar (digitToHex (i >> 4)), toChar (digitToHex (i bitand 15)))
		where
		        i						= toInt c
		        digitToHex :: !Int -> Int
		        digitToHex d
		                | d <= 9		= d + toInt '0'
		                | otherwise		= d + toInt 'A' - 10

urlDecode :: !String -> *String
urlDecode s								= mkString (urlDecode` (mkList s))
where
	urlDecode` :: ![Char] -> [Char]
	urlDecode` []						= []
	urlDecode` ['%',hex1,hex2:xs]		= [hexToChar(hex1, hex2):urlDecode` xs]
	where
		hexToChar :: !(!Char, !Char) -> Char
		hexToChar (a, b)				= toChar (hexToDigit (toInt a) << 4 + hexToDigit (toInt b))
		where
		        hexToDigit :: !Int -> Int
		        hexToDigit i
		                | i<=toInt '9'	= i - toInt '0'
		                | otherwise		= i - toInt 'A' - 10
	urlDecode` ['+':xs]				 	= [' ':urlDecode` xs]
	urlDecode` [x:xs]				 	= [x:urlDecode` xs]

