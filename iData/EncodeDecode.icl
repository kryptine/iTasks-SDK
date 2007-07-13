implementation module EncodeDecode

// encoding and decoding of information
// (c) 2005 MJP

import StdArray, StdBool, StdInt, StdList, StdOrdList, StdString, StdTuple, ArgEnv, StdMaybe, Directory
import iDataTrivial, iDataFormData, StdBimap
import GenPrint, GenParse
import dynamic_string
import EstherBackend

derive gParse UpdValue, (,,), (,)
derive gPrint UpdValue, (,,), (,)


// form submission department....

// script for transmitting name and value of changed input 

callClean :: !(Script -> ElementEvents) !Mode !String -> [ElementEvents]
callClean  onSomething Edit		_    =  [onSomething (SScript "toclean(this)")]
callClean  onSomething Submit 	myid =  [onSomething (SScript ("toclean2(" <+++ myid <+++ ")"))]
callClean  onSomething _ 		_	 =  []

submitscript :: BodyTag
submitscript 
=	BodyTag 
    [ Script [] (SScript
		(	" function toclean(inp)" +++
			" { document." +++ globalFormName +++ "." +++	updateInpName +++ ".value=inp.name+\"=\"+inp.value;" +++
			   "document." +++ globalFormName +++ ".submit(); }"
		))
	,	Script [] (SScript
		(	" function toclean2(form)" +++
			" { "  +++
				"form.hidden.value=" +++ "document." +++ globalFormName +++ "." +++ globalInpName +++ ".value;" +++
				"form.submit();" +++
			"}" 
		))

	]

// form that contains global state and empty input form for storing updated input
	
globalstateform :: !Value -> BodyTag
globalstateform  globalstate
=	Form 	[ Frm_Name globalFormName 
			, Frm_Method Post
			, Frm_Enctype "multipart/form-data"			// what to do to enable large data ??
			]
			[ Input [ Inp_Name updateInpName
					, Inp_Type Inp_Hidden
					] ""
			, Input [ Inp_Name globalInpName
					, Inp_Type Inp_Hidden
					, Inp_Value globalstate
					] ""
			]		 


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
	fromLivetime Page 			PlainString		= "N"	// encode Lifespan & StorageFormat in first character
	fromLivetime Session 		PlainString		= "S"
	fromLivetime TxtFile 		PlainString		= "P"
	fromLivetime TxtFileRO 		PlainString		= "R"
	fromLivetime DataFile 		PlainString		= "F"
	fromLivetime Database	 	PlainString		= "D"
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

DecodeHtmlStatesAndUpdate ::  (Maybe [(String, String)]) -> (![HtmlState],!Triplets)
DecodeHtmlStatesAndUpdate args
# (_,triplets,state)				= DecodeArguments args
= ([states \\states=:(id,_,_,nstate) <- DecodeHtmlStates state | id <> "" || nstate <> ""],triplets) // to be sure that no rubbish is passed on

// Parse and decode low level information obtained from server 
// In case of using a php script and external server:

DecodeArguments ::  (Maybe [(String, String)]) -> (!String,!Triplets,!String)
DecodeArguments (Just args)	
# nargs = length args
| nargs == 0 		= ("clean",[],"")
| nargs == 1		= DecodeCleanServerArguments (foldl (+++) "" [name +++ "=" +++ value +++ ";" \\ (name,value) <- args])
# tripargs 			= reverse args													// state hidden in last field, rest are triplets
# (state,tripargs)	= (urlDecode (snd (hd tripargs)),tl tripargs)					// decode state, get triplets highest positions first	
# constriplets		= filter (\(name,_) -> isSelector name) tripargs				// select constructor triplets  
# nconstriplets		= [(constrip,getSelector name) \\ (name,codedtrip) <- constriplets, (Just constrip) <- [parseString (decodeString (urlDecode codedtrip))]] // and decode
# valtriplets		= filter (\(name,_) -> not (isSelector name)) tripargs			// select all other triplets 
# nvaltriplets		= [(mytrip,new) \\ (codedtrip,new) <- valtriplets, (Just mytrip) <- [parseString (decodeString (urlDecode codedtrip))]] // and decode
# alltriplets		= ordertriplets (nconstriplets ++ nvaltriplets) []
= ("clean",determineChanged alltriplets ,state)								// order is important, first the structure than the values ...
where
	DecodeCleanServerArguments :: !String -> (!String,!Triplets,!String)			// executable, id + update , new , state
	DecodeCleanServerArguments args
	# input 							= [c \\ c <-: args | not (isControl c) ]	// get rid of communication noise
	# (thisexe,input) 					= mscan '\"'          input					// get rid of garbage
	# input								= skipping ['UD\"']   input
	# (triplet, input)					= mscan '='           input					// should give triplet
	# (found,index) 					= FindSubstr ['--']  input
	# (new,    input)					= splitAt index       input					// should give triplet value 
	# (_,input)							= mscan '='           input
	# input								= skipping ['\"GS\"'] input
	# (found,index) 					= FindSubstr ['---']  input
	# state								= if found (take index input) ['']
	# striplet	= toString triplet
	= if (striplet == "")
			("clean", [], toString state)
			(if (isSelector striplet) 
					("clean", [(fromJust` (decodeChars new) 	(parseString (decodeChars new)), "")], toString state)
					("clean", [(fromJust` (decodeChars triplet) (parseString (decodeChars triplet)) , toString new)], toString state))

	fromJust` _ (Just value) = value
	fromJust` string Nothing = ("",0,UpdI 0)

	ordertriplets [] accu = accu
	ordertriplets [x=:((id,_,_),_):xs] accu
	# (thisgroup,other) = ([x:filter (\((tid,_,_),_) -> tid == id) xs],filter (\((tid,_,_),_) -> tid <> id) xs)
	= ordertriplets other (qsort thisgroup ++ accu)

	qsort [] = []
	qsort [x=:((_,posx,_),_):xs ] = qsort [y \\ y=:((_,posy,_),_) <- xs | posy > posx] 
									++ [x] ++ 
									qsort [y \\ y=:((_,posy,_),_) <- xs | posy < posx]

	determineChanged triplets = filter updated triplets
	where
		updated ((_,_,UpdC c1),c2) 	= c1 <> c2
		updated ((_,_,UpdI i),s)  	= i <> toInt s
		updated ((_,_,UpdR r),s)  	= r <> toReal s
		updated ((_,_,UpdB True),"False")  	= True
		updated ((_,_,UpdB False),"True")  	= True
		updated ((_,_,UpdB b1),b2)  		= False
		updated ((_,_,UpdS s1),s2) 	= s1 <> s2

decodeName  name value 
| name == "hidden"	= name
| isSelector name	= decodeString value 
= decodeString name

decodeValue name value
| name == "hidden"	= value
| isSelector name	= getSelector name
= value

// traceHtmlInput utility used to see what kind of rubbish is received from client 

traceHtmlInput ::  !(Maybe [(String, String)]) -> BodyTag
traceHtmlInput args=:(Just input)
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
			, BodyTag (foldl (++) [] [[B [] "name = ", Txt (decodeName (urlDecode name) (urlDecode value)),Br,B [] "value = ", Txt (decodeValue (urlDecode name) (urlDecode value)),Br] \\ (name,value) <- input])
//			, BodyTag (foldl (++) [] [[B [] "name = ", Txt name,Br,B [] "value = ", Txt value,Br] \\ (name,value) <- input])
//			, STable [] [[Txt ("name = " <+++ name),Br,Txt ("value = " <+++ value)] \\ (name,value) <- input]
			]
where

	(htmlState,triplets)	= DecodeHtmlStatesAndUpdate args

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

writeState :: !String !String !*NWorld -> *NWorld 
writeState filename serializedstate env
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

readState :: !String !*NWorld -> (!String,!*NWorld) 
readState  filename env
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

deleteState :: !String !*NWorld -> *NWorld
deleteState  filename env
# directory								= iDataStorageDir
# ((ok,path),env) 						= pd_StringToPath (directory +++ "\\" +++ filename +++ ".txt") env
| not ok								= abort "Cannot delete indicated iData"
# (_,env)								= fremove path env
= env

// serializing and de-serializing of html states


// low level url encoding decoding of Strings

encodeString :: !String -> String
encodeString s							= /* see also urlEncode */ string_to_string52 s	// using the whole alphabet 
//encodeString s							= urlEncode s

decodeString :: !String -> *String
decodeString s							= /* see also urlDecode */ string52_to_string s	// using the whole alphabet
//decodeString s							= urlDecode s

// to encode triplets in htmlpages

encodeTriplet	:: !Triplet -> String				// encoding of triplets
encodeTriplet triplet = encodeInfo triplet

decodeTriplet	:: !String -> Maybe Triplet			// decoding of triplets
decodeTriplet triplet = decodeInfo triplet

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


