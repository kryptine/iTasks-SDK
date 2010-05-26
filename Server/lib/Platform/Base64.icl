implementation module Base64

import StdChar, StdString, StdList, StdArray, StdMisc, StdBool

//65th character is padding-character
stdAlphabet :== {'A','B','C','D','E','F','G','H','I','J','K','L','M','N','O','P','Q','R','S','T','U','V','W','X','Y','Z',
				 'a','b','c','d','e','f','g','h','i','j','k','l','m','n','o','p','q','r','s','t','u','v','w','x','y','z',
				 '0','1','2','3','4','5','6','7','8','9','+','/','='}

urlAlphabet :== {'A','B','C','D','E','F','G','H','I','J','K','L','M','N','O','P','Q','R','S','T','U','V','W','X','Y','Z',
				 'a','b','c','d','e','f','g','h','i','j','k','l','m','n','o','p','q','r','s','t','u','v','w','x','y','z',
				 '0','1','2','3','4','5','6','7','8','9','-','_','='}
			 
base64Encode :: !String -> String
base64Encode s
# l = (((size s)/3)+1)*4 //make sure we set our upperbound high-enough so no newlines are inserted.
= base64EncodeLen s l

base64EncodeLen :: !String !Length -> String
base64EncodeLen s l = addLineBreaks (encodeString s stdAlphabet) l

base64URLEncode :: !String -> String
base64URLEncode s
# l = (((size s)/3)+1)*4
= base64URLEncodeLen s l

base64URLEncodeLen :: !String !Length -> String
base64URLEncodeLen s l = addLineBreaks (encodeString s urlAlphabet) l

encodeString :: !{#Char} Alphabet -> {#Char}
encodeString s a = encodeString` s 0 a
where
	encodeString` :: !{#Char} Offset Alphabet -> {#Char}
	encodeString` s o a
	# r = (size s)-o
	| r > 3  = (encodeOctet s.[o] s.[o+1] s.[o+2] a 0) +++ (encodeString` s (o+3) a)	
	| r == 3 = (encodeOctet s.[o] s.[o+1] s.[o+2] a 0)
	| r == 2 = (encodeOctet s.[o] s.[o+1] (fromInt 0) a 1)
	| r == 1 = (encodeOctet s.[o] (fromInt 0) (fromInt 0) a 2)
	| otherwise = "" //if input is empty, return empty
	  
	encodeOctet :: !Char !Char !Char Alphabet Padding -> {#Char}
	encodeOctet c1 c2 c3 a p = encodeOctet` ((toInt c1)<<16 + (toInt c2)<<8 + (toInt c3)) (3) (createArray 4 ' ') a p
	where
		encodeOctet` :: Int Offset !*{#Char} Alphabet Padding -> *{#Char}
		encodeOctet` oct -1 s a p = s
		encodeOctet` oct off s a p
		| p > 0 = encodeOctet` (oct>>6) (off-1) {s & [off] = a.[64]} a (p-1)
		| otherwise = encodeOctet` (oct>>6) (off-1) {s & [off] = (a.[(oct bitand 63)])} a p

addLineBreaks :: !String Length -> String
addLineBreaks s l
| l > 0 = addLineBreaks` s "" l
| otherwise = abort "Length cannot be 0 or less."
where
	addLineBreaks` :: !String !String !Length -> String
	addLineBreaks` src dest len
	| len >= (size src) = (dest+++src)
	| otherwise = addLineBreaks` (src % (len,(size src))) (dest+++(src % (0,len-1))+++"\n") len

base64Decode :: !String -> String
base64Decode s = decodeString (removeLineBreaks s) 0 stdAlphabet

base64URLDecode :: !String -> String
base64URLDecode s = decodeString (removeLineBreaks s) 0 urlAlphabet

decodeString :: String Offset Alphabet -> String
decodeString s o a 
# r = (size s)-o
| r >= 4 = (decodeOctet (s % (o,o+3)) a)+++(decodeString s (o+4) a)
| r == 0 = ""
| otherwise = abort("Invalid length, size of decoding string must be a multitude of 4.")

decodeOctet :: !{#Char} Alphabet -> {#Char}
decodeOctet s a 
| (s.[2] == '=') && (s.[3] == '=') = decodeOctet` (((getValue s.[0] a)<<6+(getValue s.[1] a))>>4) 0 (createArray 1 ' ') //lose the last four obsolete bits (2*6-8b)
| s.[3] == '=' = decodeOctet` (((getValue s.[0] a)<<12+(getValue s.[1] a)<<6+(getValue s.[2] a))>>2) 1 (createArray 2 ' ') //lose the last two obsolete bits (3*6-2*8b)
| otherwise = decodeOctet` ((getValue s.[0] a)<<18+(getValue s.[1] a)<<12+(getValue s.[2] a)<<6+(getValue s.[3] a)) 2 (createArray 3 ' ')
where
	decodeOctet` :: Int Offset !*{#Char} -> *{#Char}
	decodeOctet` oct -1 s = s
	decodeOctet` oct off s = decodeOctet` (oct>>8) (off-1) {s & [off] = (fromInt (oct bitand 255))}

	getValue :: !Char Alphabet -> Int
	getValue c a = [i \\ i<-[0..(size a-2)] | (a.[i] == c)] !! 0

removeLineBreaks :: !{#Char} -> {#Char}
removeLineBreaks src = {char \\ char <-: src | char <> '\n'}
