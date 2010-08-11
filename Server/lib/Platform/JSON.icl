implementation module JSON

import StdEnv
import StdGeneric, StdMaybe, StdString
import Text
import Types

//Token type which is the intermediary representation during JSON parsing
:: Token	= TokenInt Int
			| TokenReal	Real
			| TokenString String
			| TokenBool	Bool
			| TokenNull
			| TokenBracketOpen
			| TokenBracketClose
			| TokenBraceOpen
			| TokenBraceClose
			| TokenName	String
			| TokenColon
			| TokenComma
			| TokenWhitespace String
			| TokenFail	

//Basic JSON serialization
instance toString JSONNode
where
	//Escape all strings -> make target string -> copy characters
	//The reason why first a big string is made into which the characters are copied is to
	//avoid many string concatenations with big strings
	toString node
		# node = escapeAll node
		# len = sizeOf node
		= snd (copyNode 0 node (createArray len '\0'))

//Determine serialized size of a JSON datastructure
sizeOf :: !JSONNode -> Int
sizeOf (JSONNull)		= 4
sizeOf (JSONBool True)	= 4
sizeOf (JSONBool False)	= 5
sizeOf (JSONInt x)		= size (toString x)
sizeOf (JSONReal x)		= size (toString x)
sizeOf (JSONString x)	= size x + 2
sizeOf (JSONArray x)	= let len = length x in (if (len > 0) (sum (map sizeOf x) + len - 1) 0) + 2
sizeOf (JSONObject x)	= let len = length x in (if (len > 0) (sum (map (\(l,o) -> size l + 2 + 1 + sizeOf o) x) + len - 1) 0) + 2
sizeOf (JSONRaw x)		= size x
sizeOf (JSONError)		= 0

//Escape all strings in a JSON structure
escapeAll :: !JSONNode -> JSONNode
escapeAll (JSONString s)	= JSONString (jsonEscape s)
escapeAll (JSONArray x)		= JSONArray (map escapeAll x)
escapeAll (JSONObject x)	= JSONObject (map (\(l,o) -> (l,escapeAll o)) x)
escapeAll node				= node

//Copy structure to a string
copyNode :: !Int !JSONNode !*{#Char} -> *(!Int, !*{#Char})
copyNode start (JSONNull) buffer		= (start + 4, copyChars start 0 4 "null" buffer)
copyNode start (JSONBool True) buffer	= (start + 4, copyChars start 0 4 "true" buffer)
copyNode start (JSONBool False) buffer	= (start + 5, copyChars start 0 5 "false" buffer)
copyNode start (JSONInt x) buffer		= let s = toString x in (start + size s, copyChars start 0 (size s) s buffer)
copyNode start (JSONReal x) buffer		= let s = toString x in (start + size s, copyChars start 0 (size s) s buffer)
copyNode start (JSONString x) buffer	= let len = size x in (start + len + 2 , copyChars (start + 1) 0 len x {buffer & [start] = '"', [start + len + 1] = '"'})
copyNode start (JSONArray items) buffer
	# (start,buffer)	= (start + 1, {buffer & [start] = '['})
	# (start,buffer)	= copyArrayItems start items buffer
	= (start + 1, {buffer & [start] = ']'})
where
	copyArrayItems start [] buffer = (start,buffer)
	copyArrayItems start [x] buffer = copyNode start x buffer
	copyArrayItems start [x:xs] buffer
		# (start,buffer) = copyNode start x buffer
		= copyArrayItems (start + 1) xs {buffer & [start] = ','}
copyNode start (JSONObject items) buffer
	# (start,buffer)	= (start + 1, {buffer & [start] = '{'})
	# (start,buffer)	= copyObjectItems start items buffer
	= (start + 1, {buffer &	[start] = '}'})
where
	copyObjectItems start [] buffer = (start,buffer)
	copyObjectItems start [(l,x)] buffer
		# (start,buffer) = let len = size l in (start + len + 3 , copyChars (start + 1) 0 len l {buffer & [start] = '"', [start + len + 1] = '"', [start + len + 2] = ':'})
		= copyNode start x buffer
	copyObjectItems start [(l,x):xs] buffer
		# (start,buffer) = let len = size l in (start + len + 3 , copyChars (start + 1) 0 len l {buffer & [start] = '"', [start + len + 1] = '"', [start + len + 2] = ':'})
		# (start,buffer) = copyNode start x buffer
		= copyObjectItems (start + 1) xs {buffer & [start] = ','}
copyNode start (JSONRaw x) buffer	= (start + size x, copyChars start 0 (size x) x buffer) 	
copyNode start _ buffer				= (start,buffer)

copyChars offset i num src dst
	| i == num		= dst
	| otherwise		= copyChars offset (inc i) num src {dst & [offset + i] = src.[i]}

//Basic JSON deserialization (just structure)
instance fromString JSONNode
where
	//Lex -> remove whitespace -> parse 
	fromString s = fst (parse (removeWhitespace (snd (lex s 0 []))))

lex :: String Int [Token] -> (Int, [Token])
lex input offset tokens
	| offset >= size input	= (offset, reverse tokens) 				//Done
							= lex input newOffset [token:tokens]	//Lex another token and do recursive call
where
	(newOffset, token)		= lexAny input offset lexFunctions
	lexFunctions			= [ lexBracketOpen
							  , lexBracketClose
							  , lexBraceOpen
							  , lexBraceClose
							  , lexColon
							  , lexComma
							  , lexNull
							  , lexTrue
							  , lexFalse
							  , lexWhitespace
							  , lexNumber
							  , lexString
							  ]
	//Try any of the lexers in the list until one succeeds
	lexAny :: String Int [(String Int -> Maybe (Int, Token))] -> (Int, Token) 
	lexAny input offset [] = (size input, TokenFail)
	lexAny input offset [f:fs] = case f input offset of
		(Just result)	= result
		(Nothing)		= lexAny input offset fs
	
	//Lex token of fixed size
	lexFixed chars token input offset
		| input % (offset,offset + (size chars) - 1) == chars	= Just (offset + (size chars), token)
																= Nothing
	//Single character lex functions													
	
	lexBracketOpen	= lexFixed "[" TokenBracketOpen
	lexBracketClose	= lexFixed "]" TokenBracketClose
	lexBraceOpen	= lexFixed "{" TokenBraceOpen
	lexBraceClose	= lexFixed "}" TokenBraceClose
	lexColon		= lexFixed ":" TokenColon
	lexComma		= lexFixed "," TokenComma
	
	//Fixed width lex functions
	
	lexNull			= lexFixed "null" TokenNull
	lexTrue			= lexFixed "true" (TokenBool True)
	lexFalse		= lexFixed "false" (TokenBool False)
	
	//Variable width lex functions
	
	//Whitespace
	lexWhitespace input offset
		| last == offset	= Nothing
							= Just (last, TokenWhitespace (input % (offset,last - 1)))
	where
		last = findEnd isSpace input offset
	//Numbers
	lexNumber input offset
		| intpart == offset	= Nothing
		| otherwise
			| fracpart == intpart	= Just (intpart, TokenInt (toInt (input % (offset,intpart - 1))))
			| otherwise
				| exppart == fracpart	= Just (fracpart, TokenReal (toReal (input % (offset, fracpart - 1))))
				| otherwise				= Just (exppart, TokenReal (toReal (input % (offset, exppart - 1))))
	where	
		intpart		= findEnd isDigit input (optMin input offset)
		fracpart	= optFrac input intpart
		exppart		= optExp input fracpart

	//If the current char is a -, advance the offset by one
	optMin input offset
		| offset >= size input			= offset
		| input.[offset] == '-'			= offset + 1
										= offset
	//If the current char is a '.' advance the offset as long as we find digits
	optFrac input offset
		| offset >= size input			= offset
		| input.[offset] == '.'			= findEnd isDigit input (offset + 1)
										= offset
	//If the current char is 'e' or 'E' advance the offset as long as we can
	//after an optional '-'. If we can't advance after the 'e' stop.
	optExp input offset
		| offset >= size input			= offset
		| input.[offset] == 'e' || input.[offset] == 'E'
			| end == offset + 1			= offset
			| otherwise					= end
		| otherwise						= offset
	where
		end	= findEnd isDigit input (optMin input (offset + 1))
		
	//Find the first offset where the predicate no longer holds					
	findEnd pred input offset
			| offset >= size input		= offset
			| pred input.[offset]		= findEnd pred input (offset + 1)
										= offset
	//String
	lexString input offset
		| offset >= size input			= Nothing
		| input.[offset] <> '"'			= Nothing
										= Just (end, TokenString (input % (offset + 1, end - 2)))
	where
		end = findStringEnd input (offset + 1)
		
		findStringEnd input offset
			| offset >= size input		= offset
			| input.[offset] == '"'		= offset + 1
			| input.[offset] == '\\'	= findStringEnd input (offset + 2) //Skip the escaped character
										= findStringEnd input (offset + 1)
		
//Whitespace removal
removeWhitespace :: [Token] -> [Token]
removeWhitespace l = filter (not o isWhitespaceToken) l

isWhitespaceToken :: Token -> Bool
isWhitespaceToken (TokenWhitespace _)	= True
isWhitespaceToken _						= False

//Simple recursive descent parser
parse :: ![Token] -> (!JSONNode,![Token])
parse [TokenNull:ts] 							= (JSONNull, ts)
parse [TokenBool x:ts] 							= (JSONBool x, ts)
parse [TokenInt x:ts]							= (JSONInt x, ts)
parse [TokenReal x:ts] 							= (JSONReal x, ts)
parse [TokenString x:ts]						= (JSONString (jsonUnescape x), ts)
parse [TokenBracketOpen,TokenBracketClose:ts]	= (JSONArray [], ts)
parse [TokenBracketOpen:ts]
	= case (parseArrayItems ts []) of
		([TokenBracketClose:ts`],items)	= (JSONArray (reverse items), ts`)
		_								= (JSONError, ts)
where
	parseArrayItems :: ![Token] ![JSONNode] -> (![Token],![JSONNode])
	parseArrayItems tokens nodes
		= case (parse tokens) of
			(node,[TokenComma:ts])	= parseArrayItems ts [node:nodes]
			(node,ts)				= (ts,[node:nodes])
parse [TokenBraceOpen:TokenBraceClose:ts]		= (JSONObject [], ts)
parse [TokenBraceOpen:ts]
	= case (parseObjectItems ts []) of
		([TokenBraceClose:ts`],items)	= (JSONObject (reverse items), ts`)
		_								= (JSONError, ts)
where
	parseObjectItems :: ![Token] ![(!String,!JSONNode)] -> (![Token],![(!String,!JSONNode)])
	parseObjectItems tokens nodes
		= case (parse tokens) of
			(JSONString label,[TokenColon:ts])
				= case (parse ts) of
					(node,[TokenComma:ts`])	= parseObjectItems ts` [(label,node):nodes]
					(node,ts`)				= (ts`,[(label,node):nodes])
			_
				= (tokens,nodes)
parse tokens = (JSONError,tokens)


//Escape a string
jsonEscape :: !String -> String
jsonEscape src = copyChars 0 0 reps src (createArray (size src + length reps) '\0')
where
	reps	= findChars 0 src	
	//Find the special characters
	findChars :: Int String -> [(!Int,!Char)]
	findChars i s
		| i >= size s 	= []
		| c == '\\' || c == '"' || c == '/' || c == '\b' || c == '\f' || c == '\n' || c == '\r' || c == '\t'
			= [(i,c): findChars (i + 1) s] 
			= findChars (i + 1) s
		where 
			c = s.[i]
	//Build the escaped string from the original and the replacements		
	copyChars :: Int Int [(!Int, !Char)] String *String -> *String
	copyChars is id [] src dest
		| is < size src		=	copyChars (is + 1) (id + 1) [] src {dest & [id] = src.[is]}
							=	dest
	copyChars is id reps=:[(ir,c):rs] src dest
		| is == ir			=	copyChars (is + 1) (id + 2) rs src {dest & [id] = '\\', [id + 1] = rep c}
							=	copyChars (is + 1) (id + 1) reps src {dest & [id] = src.[is]}
		where
			rep '\\'	= '\\'
			rep '"'		= '"'
			rep '/'		= '/'
			rep '\b'	= 'b'
			rep '\f'	= 'f'
			rep '\n'	= 'n'
			rep '\r'	= 'r'
			rep '\t'	= 't'
			
//Unescape a string
jsonUnescape :: !String -> String
jsonUnescape s = unescape` s 0
where
	unescape` s offset
		| offset >= size s	= s
		| s.[offset] == '\\'
			| offset + 1 >= size s		= s
			| otherwise = tryReplace s (offset + 1) controlChars
		| otherwise			= unescape` s (offset + 1)
	
	tryReplace s offset []	= unescape` s offset 
	tryReplace s offset [(c,r):xs]
		| s.[offset] == c	= s % (0, offset - 2) +++ r +++ jsonUnescape (s % (offset + 1, size s))
		| otherwise			= tryReplace s offset xs
		
	controlChars = [('\\',"\\"),('"',"\""),('/',"/"),('b',"\b"),('f',"\f"),('n',"\n"),('t',"\t")]

//Intersperse an element on a list
intersperse :: a [a] -> [a]
intersperse i [] = []
intersperse i [x] = [x]
intersperse i [x:xs] = [x,i:intersperse i xs]

//-------------------------------------------------------------------------------------------

toJSON :: a -> JSONNode | JSONEncode{|*|} a
toJSON x = case (JSONEncode{|*|} x) of
	[node]	= node
	_		= JSONError 

/*
* Generic JSON encoder
*/
generic JSONEncode t :: t -> [JSONNode]

JSONEncode{|Int|} x = [JSONInt x]
JSONEncode{|Real|} x = [JSONReal x]
JSONEncode{|Char|} x = [JSONString {x}]
JSONEncode{|Bool|} x = [JSONBool x]
JSONEncode{|String|} x = [JSONString x]
JSONEncode{|UNIT|} (UNIT) = []
JSONEncode{|PAIR|} fx fy (PAIR x y) = fx x ++ fy y
JSONEncode{|EITHER|} fx fy (LEFT x) = fx x
JSONEncode{|EITHER|} fx fy (RIGHT y) = fy y
JSONEncode{|OBJECT|} fx (OBJECT x) = fx x
JSONEncode{|CONS of d|} fx (CONS x)
	//Record
	| length d.gcd_fields <> 0	= [JSONObject [ (f.gfd_name, o)\\ o <- fx x & f <- d.gcd_fields ]]
	//Constructor without parameters
	| d.gcd_arity == 0			= [JSONString d.gcd_name]
	//Constructor with parameters				
	| otherwise					= [JSONArray [JSONString d.gcd_name : fx x]]
	
JSONEncode{|FIELD of d|} fx (FIELD x) = fx x							
JSONEncode{|[]|} fx x = [JSONArray (flatten [fx e \\ e <- x])]
JSONEncode{|(,)|} fx fy (x,y) = [JSONArray (fx x ++ fy y)]
JSONEncode{|(,,)|} fx fy fz (x,y,z) = [JSONArray (fx x ++ fy y ++ fz z)]
JSONEncode{|(,,,)|} fx fy fz fi (x,y,z,i) = [JSONArray (fx x ++ fy y ++ fz z ++ fi i)]
JSONEncode{|(,,,,)|} fx fy fz fi fj (x,y,z,i,j) = [JSONArray (fx x ++ fy y ++ fz z ++ fi i ++ fj j)]
JSONEncode{|{}|} fx x = [JSONArray (flatten [fx e \\ e <-: x])]
JSONEncode{|{!}|} fx x = [JSONArray (flatten [fx e \\ e <-: x])]
JSONEncode{|Maybe|} fx (Just x) = fx x
JSONEncode{|Maybe|} fx (Nothing) = [JSONNull]
JSONEncode{|JSONNode|} node = [node]

//-------------------------------------------------------------------------------------------
fromJSON :: JSONNode -> Maybe a | JSONDecode{|*|} a
fromJSON node = fst (JSONDecode{|*|} [node])

/*
* Generic JSON parser, using a list of tokens
*/
generic JSONDecode t :: [JSONNode] -> (!Maybe t, ![JSONNode])

JSONDecode{|Int|} [JSONInt i:xs]		= (Just i, xs)
JSONDecode{|Int|} l						= (Nothing, l)

JSONDecode{|Real|} [JSONReal r:xs]		= (Just r, xs)
JSONDecode{|Real|} [JSONInt i:xs]		= (Just (toReal i), xs)
JSONDecode{|Real|} l					= (Nothing, l)

JSONDecode{|Char|} l =: [JSONString s:xs]
	| size s == 1						= (Just s.[0],xs)
										= (Nothing, l)
JSONDecode{|Char|} l					= (Nothing, l)

JSONDecode{|Bool|} [JSONBool b:xs]		= (Just b,xs)
JSONDecode{|Bool|} l					= (Nothing, l)

JSONDecode{|String|} [JSONString s:xs]	= (Just s, xs)
JSONDecode{|String|} l					= (Nothing, l)

JSONDecode{|UNIT|} l					= (Just UNIT, l)

JSONDecode{|PAIR|} fx fy l = case fx l of
	(Just x,xs)	= case fy xs of
		(Just y, ys)			= (Just (PAIR x y), ys)
		_						= (Nothing, l)
	_							= (Nothing, l)
	
JSONDecode{|EITHER|} fx fy l = case fx l of
	(Just x, xs)				= (Just (LEFT x),xs)
	(Nothing, xs)				= case fy l of
		(Just y, ys)			= (Just (RIGHT y),ys)
		(Nothing, ys)			= (Nothing, l)

JSONDecode{|OBJECT|} fx l = case fx l of
	(Just x, xs)	= (Just (OBJECT x),xs)
	_				= (Nothing, l)

JSONDecode{|CONS of d|} fx l
	//Records
	| length d.gcd_fields <> 0	= case l of
		[JSONObject fields: xs] = case fx [JSONObject fields] of
			(Just x, _)						= (Just (CONS x),xs)
			_								= (Nothing, l)
		_									= (Nothing, l)
	//Constructor without parameters
	| d.gcd_arity == 0			= case l of
		[JSONString name: xs]
			| name == d.gcd_name			= case fx xs of
				(Just x, ys)				= (Just (CONS x),ys)
				_							= (Nothing, l)
			| otherwise						= (Nothing, l)
		_									= (Nothing, l)
	//Constructor with parameters
	| otherwise					= case l of
		[JSONArray [JSONString name:fields] :xs]
			| name == d.gcd_name			= case fx fields of
				(Just x, _)					= (Just (CONS x), xs)
				_							= (Nothing, l)
			| otherwise						= (Nothing, l)
		_									= (Nothing, l)
		
JSONDecode{|CONS|} fx l = (Nothing, l)

JSONDecode{|FIELD of d|} fx l =: [JSONObject fields]
	= case findField d.gfd_name fields of
		(Just field)	= case fx [field] of
			(Just x, _)	= (Just (FIELD x), l)
			_			= (Nothing, l)
		_				= (Nothing, l)
where
	findField match [] 	= Nothing
	findField match [(l,x):xs]
		| l == match 	= Just x
						= findField match xs
						
JSONDecode{|FIELD|} fx l = (Nothing, l)

JSONDecode{|[]|} fx l =:[JSONArray items:xs]
	= case decodeItems fx items of
		(Just x)		= (Just x, xs)
		_				= (Nothing, l)
JSONDecode{|[]|} fx l 	= (Nothing, l)

JSONDecode{|(,)|} fx fy l =:[JSONArray [xo,yo]:xs]
	= case fx [xo] of
		(Just x,_)	= case fy [yo] of
			(Just y,_)		= (Just (x,y), xs)
			_				= (Nothing, l)
		_					= (Nothing, l)
JSONDecode{|(,)|} fx fy l	= (Nothing, l)

JSONDecode{|(,,)|} fx fy fz l =:[JSONArray [xo,yo,zo]:xs]
	= case fx [xo] of
		(Just x,_)	= case fy [yo] of
			(Just y,_)			= case fz [zo] of
				(Just z,_)		= (Just (x,y,z), xs)
				_				= (Nothing, l)
			_					= (Nothing, l)
		_						= (Nothing, l)
JSONDecode{|(,,)|} fx fy fz l	= (Nothing, l)

JSONDecode{|(,,,)|} fx fy fz fi l =:[JSONArray [xo,yo,zo,io]:xs]
	= case fx [xo] of
		(Just x,_)	= case fy [yo] of
			(Just y,_)	= case fz [zo] of
				(Just z,_) = case fi [io] of
					(Just i,_)		= (Just (x,y,z,i), xs)
					_				= (Nothing, l)
				_					= (Nothing, l)
			_						= (Nothing, l)
		_							= (Nothing, l)
JSONDecode{|(,,,)|} fx fy fz fi l	= (Nothing, l)

JSONDecode{|(,,,,)|} fx fy fz fi fj l =:[JSONArray [xo,yo,zo,io,jo]:xs]
	= case fx [xo] of
		(Just x,_)	= case fy [yo] of
			(Just y,_)	= case fz [zo] of
				(Just z,_) = case fi [io] of
					(Just i,_)	= case fj [jo] of
						(Just j,_)		= (Just (x,y,z,i,j), xs)
						_				= (Nothing, l)
					_					= (Nothing, l)
				_						= (Nothing, l)
			_							= (Nothing, l)
		_								= (Nothing, l)
JSONDecode{|(,,,,)|} fx fy fz fi fj l	= (Nothing, l)

JSONDecode{|{}|} fx l =:[JSONArray items:xs]
	= case decodeItems fx items of
		(Just x)		= (Just {e \\ e <- x}, xs)
		_				= (Nothing, l)
JSONDecode{|{}|} fx l 	= (Nothing, l)

JSONDecode{|{!}|} fx l =:[JSONArray items:xs]
	= case decodeItems fx items of
		(Just x)		= (Just {e \\ e <- x}, xs)
		_				= (Nothing, l)
JSONDecode{|{!}|} fx l 	= (Nothing, l)

decodeItems fx [] 		= Just []
decodeItems fx [ox:oxs]	= case fx [ox] of
	(Just x, _)	= case decodeItems fx oxs of
		(Just xs)	= Just [x:xs]
		_ 			= Nothing
	_			= Nothing

JSONDecode{|Maybe|} fx [JSONNull:xs]	= (Just Nothing, xs)
JSONDecode{|Maybe|} fx l = case fx l of
	(Just x,xs)							= (Just (Just x), xs)
	_									= (Nothing,l)

JSONDecode{|JSONNode|} [x:xs]			= (Just x, xs)
JSONDecode{|JSONNode|} l				= (Nothing, l)

jsonQuery :: !String !JSONNode -> Maybe a | JSONDecode{|*|} a
jsonQuery path node
	= case (findNode (split "/" path) node ) of
		Just child	= fromJSON child
		Nothing		= Nothing
where
	findNode :: ![String] !JSONNode -> (Maybe JSONNode)
	findNode [] node	= Just node
	findNode [s:ss] (JSONObject fields)
		= case findField s fields of
			Just f	= findNode ss f
			Nothing	= Nothing
	findNode [s:ss] (JSONArray items)
		# index = toInt s
		| index >= 0 && index < length items	= findNode ss (items !! index)
		| otherwise								= Nothing
	findNode _ _		= Nothing
	
	findField s []			= Nothing
	findField s [(l,x):xs]	= if (l == s) (Just x) (findField s xs)
	