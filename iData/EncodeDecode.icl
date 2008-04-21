implementation module EncodeDecode

// encoding and decoding of information
// (c) 2005 MJP

import StdArray, StdBool, StdInt, StdList, StdOrdList, StdString, StdTuple, ArgEnv, StdMaybe, Directory
import iDataTrivial, iDataFormData, StdBimap
import GenPrint, GenParse
import dynamic_string
import EstherBackend
import HttpTextUtil
//import sapldebug

derive gParse UpdValue, (,,), (,)
derive gPrint UpdValue, (,,), (,)


// form submission department....

// script for transmitting name and value of changed input 

callClean :: !(Script -> ElementEvents) !Mode !String !Lifespan !Bool -> [ElementEvents]
callClean event	mode	elemid	lsp	action
| isMember mode [Edit, Submit]	= [event (SScript ("toClean(this,'" +++ elemid +++ "'," +++ isAction action +++ "," +++ isSubmit mode +++ "," +++ isOnClient lsp +++ ")"))]
| otherwise						= []
where
	isAction True 		= "true"
	isAction False		= "false"

	isSubmit Submit		= "true"
	isSubmit _			= "false"

	isOnClient Client 	= "true"
	isOnClient _ 		= "false"

isSelector name 	= name%(0,size selectorInpName - 1) == selectorInpName
getSelector name 	= decodeString (name%(size selectorInpName,size name - 1))

// Serializing Html states...

EncodeHtmlStates :: ![HtmlState] -> String
EncodeHtmlStates [] = "$"
EncodeHtmlStates [(id,lifespan,storageformat,state):xsys] 
	= encodeString
	  (	"(\"" +++ 										// begin mark
		fromLivetime lifespan storageformat +++ 		// character encodes lifetime and kind of encoding
		id +++ 											// id of state 
	  	"\"," +++ 										// delimiter
	  	 state +++ 										// encoded state 
	  	")" 
	  )	+++
	  "$" +++ 											// end mark
	  EncodeHtmlStates xsys
where
	fromLivetime Client			PlainString		= "C"	// encode Lifespan & StorageFormat in first character
	fromLivetime Page 			PlainString		= "N"	// encode Lifespan & StorageFormat in first character
	fromLivetime Session 		PlainString		= "S"
	fromLivetime TxtFile 		PlainString		= "P"
	fromLivetime TxtFileRO 		PlainString		= "R"
	fromLivetime DataFile 		PlainString		= "F"
	fromLivetime Database	 	PlainString		= "D"
	fromLivetime Client			StaticDynamic	= "c"
	fromLivetime Page 			StaticDynamic	= "n"
	fromLivetime Session 		StaticDynamic	= "s"
	fromLivetime TxtFile 		StaticDynamic	= "p"
	fromLivetime TxtFileRO 		StaticDynamic	= "r"
	fromLivetime DataFile 		StaticDynamic	= "f"
	fromLivetime Database	 	StaticDynamic	= "d"

// de-serialize Html State

DecodeHtmlStates :: !String -> [HtmlState]
DecodeHtmlStates state					= toHtmlState` (mkList state)
where
	toHtmlState` :: ![Char] -> [HtmlState]
	toHtmlState` [] 					= []
	toHtmlState` listofchar				= [mkHtmlState (mkList (decodeChars first)) : toHtmlState` second]
	where
		(first,second)					= mscan '$' listofchar									// search for end mark

		mkHtmlState :: ![Char] -> HtmlState
		mkHtmlState	elem				= ( mkString (stl fid)									// decode unique identification
										  , lifespan											// decode livetime from character
										  , format												// decode storage format from character
										  , mkString (stl (reverse (stl (reverse formvalue)))) 	// decode state
										  )
		where
			(fid,formvalue)				= mscan '"' (stl (stl elem)) 							// skip '("'
			(lifespan,format)			= case fid of
											['C':_]		= (Client,      PlainString  )
											['c':_]		= (Client,      StaticDynamic)
											['N':_]		= (Page,        PlainString  )
											['n':_]		= (Page,        StaticDynamic)
											['S':_]		= (Session,     PlainString  )
											['s':_] 	= (Session,     StaticDynamic)
											['P':_] 	= (TxtFile,  	PlainString  )
											['p':_]		= (TxtFile,  	StaticDynamic)
											['R':_]		= (TxtFileRO,	PlainString  )
											['r':_] 	= (TxtFileRO,	StaticDynamic)
											['D':_] 	= (Database,    PlainString  )
											['d':_] 	= (Database,    StaticDynamic)
											['F':_] 	= (DataFile,    PlainString  )
											['f':_] 	= (DataFile,    StaticDynamic)
											_			= (Page,        PlainString  )

// reconstruct HtmlState out of the information obtained from browser

DecodeHtmlStatesAndUpdate ::  [(String, String)] -> (![HtmlState],!Triplets,!String)
DecodeHtmlStatesAndUpdate args
# (_,triplets,state,focus)			= DecodeArguments args
= ([states \\states=:(id,_,_,nstate) <- DecodeHtmlStates state | id <> "" || nstate <> ""],triplets, focus) // to be sure that no rubbish is passed on

// Decode posted form information obtained from http layer 
DecodeArguments ::  [(String, String)] -> (!String,!Triplets,!String,!String)
DecodeArguments args
	# state				= http_getValue "GS" args ""
	# focus				= http_getValue "FS" args ""
	# tripargs			= [decodeNameValue (n,v) \\ (n,v) <- args | not (isMember n ["GS","FS"])]
	# triplets			= ordertriplets [(triplet,value) \\ (mbtriplet,value) <- tripargs, Just triplet <- [decodeTriplet mbtriplet]] [] // order is important, first the structure than the values ...
	# triplets			= determineChanged triplets
	= ("clean", triplets, state, focus)
where
						
	fromJustTriplet :: (Maybe Triplet) -> Triplet
	fromJustTriplet Nothing 		= ("",0,UpdI 0)
	fromJustTriplet (Just triplet)	= triplet
						
	ordertriplets [] accu = accu
	ordertriplets [x=:((id,_,_),_):xs] accu
	# (thisgroup,other) = ([x:filter (\((tid,_,_),_) -> tid == id) xs],filter (\((tid,_,_),_) -> tid <> id) xs)
	= ordertriplets other (qsort thisgroup ++ accu)

	qsort [] = []
	qsort [x=:((_,posx,_),_):xs ] = qsort [y \\ y=:((_,posy,_),_) <- xs | posy > posx] 
									++ [x] ++ 
									qsort [y \\ y=:((_,posy,_),_) <- xs | posy < posx]

	determineChanged :: [TripletUpdate] -> [TripletUpdate]
	determineChanged triplets = filter updated triplets
	where
		updated ((_,_,UpdC c1),c2) 			= c1 <> c2
		updated ((_,_,UpdI i),s)  			= i <> toInt s
		updated ((_,_,UpdR r),s)  			= r <> toReal s
		updated ((_,_,UpdB True),"False")  	= True
		updated ((_,_,UpdB False),"True")  	= True
		updated ((_,_,UpdB b1),b2)  		= False
		updated ((_,_,UpdS s1),s2) 			= s1 <> s2

decodeNameValue :: !(!String,!String) -> (!String,!String)
decodeNameValue (name,value)
	| isSelector name	= (value, getSelector name)
	| otherwise			= (name, value)

// traceHtmlInput utility used to see what kind of rubbish is received from client 
traceHtmlInput ::  [(String, String)] -> BodyTag
traceHtmlInput args
=	BodyTag	[ Br, B [] "State values received from client when application started:", Br,
				STable [] [ [B [] "Triplets:",Br]
							, showTriplet triplets
						  ,[B [] "Id:", B [] "Lifespan:", B [] "Format:", B [] "Value:"]
						: [  [Txt id, Txt (showl life), Txt ( showf storage), Txt (shows storage state)] 
						  \\ (id,life,storage,state) <- htmlState
						  ]
						]
			, Br
			, B [] "Undecoded information from client received:", Br, Br
			, BodyTag (foldl (++) [] [[B [] "name = ", Txt (fst (decodeNameValue (name,value))),Br,B [] "value = ", Txt (snd (decodeNameValue (name,value))),Br] \\ (name,value) <- args])
			]
where

	(htmlState,triplets,focus)	= DecodeHtmlStatesAndUpdate args

	showTriplet triplets	= [STable [] [[Txt (printToString triplet)] \\ triplet <- triplets]]
	showl life				= toString life
	showf storage			= case storage of PlainString -> "String";  _ -> "S_Dynamic"
	shows PlainString s		= s
	shows StaticDynamic d	= toStr (string_to_dynamic` d)											// "cannot show dynamic value" 


	toStr dyn = ShowValueDynamic dyn <+++ " :: " <+++ ShowTypeDynamic dyn

	string_to_dynamic` :: {#Char} -> Dynamic	// just to make a unique copy as requested by string_to_dynamic
	string_to_dynamic` s	= string_to_dynamic {s` \\ s` <-: s}
	
	strip s = { ns \\ ns <-: s | ns >= '\020' && ns <= '\0200'}
	
	ShowValueDynamic :: Dynamic -> String
	ShowValueDynamic d = strip (foldr (+++) "" (fst (toStringDynamic d)) +++ " ")
	
	ShowTypeDynamic :: Dynamic -> String
	ShowTypeDynamic d = strip (snd (toStringDynamic d) +++ " ")

// writing and reading of persistent states to a file

writeStateFile :: !String !String !*NWorld -> *NWorld 
writeStateFile filename serializedstate env
# ((ok,mydir),env) = pd_StringToPath iDataStorageDir env
| not ok = abort ("writeState: cannot create path to " +++ iDataStorageDir)
#(_,env)		= case getFileInfo mydir env of
						((DoesntExist,fileinfo),env)	= createDirectory mydir env
						(_,env)							= (NoDirError,env)
# (ok,file,env)	= fopen (iDataStorageDir +++ "/" +++ filename +++ ".txt") FWriteData env
| not ok	 	= env
# file			= fwrites serializedstate file  // DEBUG
# (ok,env)		= fclose file env
= env

readStateFile :: !String !*NWorld -> (!String,!*NWorld) 
readStateFile  filename env
# ((ok,mydir),env) = pd_StringToPath iDataStorageDir env
| not ok = abort ("readState: cannot create path to " +++ iDataStorageDir)
#(_,env)		= case getFileInfo mydir env of
						((DoesntExist,fileinfo),env)	= createDirectory mydir env
						(_,env)							= (NoDirError,env)
# (ok,file,env)	= fopen (iDataStorageDir +++ "/" +++ filename +++ ".txt") FReadData env
| not ok 		= ("",env)
# (string,file)	= freads file big
| not ok 		= ("",env)
# (ok,env)		= fclose file env
= (string,env)
where
	big			= 1000000

deleteStateFile :: !String !*NWorld -> *NWorld
deleteStateFile  filename env
# directory								= iDataStorageDir
# ((ok,path),env) 						= pd_StringToPath (directory +++ "\\" +++ filename +++ ".txt") env
| not ok								= abort "Cannot delete indicated iData"
# (_,env)								= fremove path env
= env

// serializing and de-serializing of html states


// low level url encoding decoding of Strings

encodeString :: !String -> String
encodeString s							= string_to_string52 s

decodeString :: !String -> *String
decodeString s							= string52_to_string s


// to encode triplets in htmlpages

encodeTriplet	:: !Triplet -> String				// encoding of triplets
encodeTriplet triplet = encodeInfo triplet

decodeTriplet	:: !String -> Maybe Triplet			// decoding of triplets
decodeTriplet triplet = decodeInfo triplet

// encodes only the formid and counter to use as identifier in html pages
encodeInputId	:: !Triplet -> String
encodeInputId (formid, cntr, updval) = encodeInfo (formid, cntr)

// utility functions based on low level encoding - decoding

encodeInfo :: !a -> String | gPrint{|*|} a
encodeInfo inp							= encodeString (printToString inp)

decodeInfo :: !String -> Maybe a | gParse{|*|} a
decodeInfo str							= parseString (decodeString str)

decodeChars :: ![Char] -> *String
decodeChars cs							= decodeString (mkString cs)

// compact John van Groningen encoding-decoding to lower and uppercase alpabeth

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


// trace to file ...

trace_to_file :: !String !*World -> *World
trace_to_file s world
	# (ok,file,world) = fopen TraceFile FAppendText world
	| not ok
		= abort ("Could not open "+++ TraceFile)
	# file = fwrites s file
	# file = fwritec '\n' file
	# (ok,world) = fclose file world
	| not ok
		= abort ("Could not close "+++ TraceFile)
	= world

