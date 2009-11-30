implementation module JSON

import StdEnv
import StdGeneric, StdMaybe
import Text

//-------------------------------------------------------------------------------------------
toJSON :: a -> String | JSONEncode{|*|} a
toJSON x = join "" (JSONEncode{|*|} x [])

/*
* Generic JSON printer, using a list of strings as continuation
*/
generic JSONEncode t :: t [String] -> [String]

JSONEncode{|Int|} x c = [toString x:c]
JSONEncode{|Real|} x c = [toString x:c]
JSONEncode{|Char|} x c = [toString x:c]
JSONEncode{|Bool|} True c = ["true":c]
JSONEncode{|Bool|} False c = ["false":c]
JSONEncode{|String|} x c = ["\"",jsonEscape x,"\"":c]
JSONEncode{|UNIT|} (UNIT) c = c
JSONEncode{|PAIR|} fx fy (PAIR x y) c = fx x [", " : fy y c]
JSONEncode{|EITHER|} fx fy (LEFT x) c = fx x c
JSONEncode{|EITHER|} fx fy (RIGHT y) c = fy y c
JSONEncode{|OBJECT|} fx (OBJECT x) c = fx x c
JSONEncode{|CONS of d|} fx (CONS x) c
	| length d.gcd_fields <> 0	= ["{": fx x ["}":c]] 						//Record
	| d.gcd_arity == 0			= ["\"",d.gcd_name,"\"":c]					//Constructor without parameters
								= ["[\"",d.gcd_name,"\", ": fx x ["]":c]]	//Constructor with parameters
	
JSONEncode{|FIELD of d|} fx (FIELD x) c = ["\"", d.gfd_name, "\" : " : fx x c]							
JSONEncode{|[]|} fx x c = JSONEncodeList fx x c
JSONEncode{|(,)|} fx fy (x,y) c = ["[": fx x [",": fy y ["]":c]]]
JSONEncode{|{}|} fx x c = JSONEncodeList fx [e \\ e <-: x] c
JSONEncode{|{!}|} fx x c = JSONEncodeList fx [e \\ e <-: x] c
JSONEncode{|Maybe|} fx (Just x) c = fx x c
JSONEncode{|Maybe|} fx (Nothing) c = ["null":c]
JSONEncode{|JSON|} (JSON s) c = [s:c]

//List generation for lists and arrays
JSONEncodeList fx x c = ["[": ( flatten ( intersperse [","] (map (flip fx []) x)) ) ++ ["]": c]]

//Escape a string
jsonEscape :: String -> String
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
unescape :: String -> String
unescape s = unescape` s 0
where
	unescape` s offset
		| offset >= size s	= s
		| s.[offset] == '\\'
			| offset + 1 >= size s		= s
			| otherwise = tryReplace s (offset + 1) controlChars
		| otherwise			= unescape` s (offset + 1)
	
	tryReplace s offset []	= unescape` s offset 
	tryReplace s offset [(c,r):xs]
		| s.[offset] == c	= s % (0, offset - 2) +++ r +++ unescape (s % (offset + 1, size s))
		| otherwise			= tryReplace s offset xs
		
	controlChars = [('\\',"\\"),('"',"\""),('/',"/"),('b',"\b"),('f',"\f"),('n',"\n"),('t',"\t")]

//Intersperse an element on a list
intersperse :: a [a] -> [a]
intersperse i [] = []
intersperse i [x] = [x]
intersperse i [x:xs] = [x,i:intersperse i xs]

//-------------------------------------------------------------------------------------------
fromJSON :: String -> Maybe a | JSONDecode{|*|} a
fromJSON input = fst (JSONDecode{|*|} (removeWhitespace (snd (lex input 0 []))))

removeWhitespace :: [Token] -> [Token]
removeWhitespace l = filter (not o isWhitespaceToken) l

isWhitespaceToken :: Token -> Bool
isWhitespaceToken (TokenWhitespace _)	= True
isWhitespaceToken _						= False

lex :: String Int [Token] -> (Int, [Token])
lex input offset tokens
	| offset >= size input	= (offset, reverse tokens) 				//Done
							= lex input newOffset [token:tokens]	//Lex another token and recurse
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
		
		//TODO: Deal with escaped characters
/*
* Generic JSON parser, using a list of tokens
*/
generic JSONDecode t :: [Token] -> (Maybe t, [Token])

JSONDecode{|Int|} [TokenInt i:xs]		= (Just i, xs)
JSONDecode{|Int|} l						= (Nothing, l)

JSONDecode{|Real|} [TokenReal r:xs]		= (Just r, xs)
JSONDecode{|Real|} [TokenInt i:xs]		= (Just (toReal i), xs)
JSONDecode{|Real|} l					= (Nothing, l)

JSONDecode{|Char|} l =: [TokenString s:xs]
	| size s == 1						= (Just s.[0],xs)
										= (Nothing, l)
JSONDecode{|Char|} l					= (Nothing, l)

JSONDecode{|Bool|} [TokenBool b:xs]		= (Just b,xs)
JSONDecode{|Bool|} l					= (Nothing, l)

JSONDecode{|String|} [TokenString s:xs]	= (Just (unescape s), xs)
JSONDecode{|String|} l					= (Nothing, l)

JSONDecode{|UNIT|} l					= (Just UNIT, l)

JSONDecode{|PAIR|} fx fy l = case fx l of
	(Just x,[TokenComma :xs])	= case fy xs of
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
	| length d.gcd_fields <> 0	= case l of
		[TokenBraceOpen: xs] = case fx xs of
			(Just x, [TokenBraceClose :ys])	= (Just (CONS x),ys)
			_								= (Nothing, l)
		_									= (Nothing, l)
	| d.gcd_arity == 0			= case l of
		[TokenString name: xs]
			| name == d.gcd_name			= case fx xs of
				(Just x, ys)				= (Just (CONS x),ys)
				_							= (Nothing, l)
			| otherwise						= (Nothing, l)
		_									= (Nothing, l)
	| otherwise					= case l of
		[TokenBracketOpen, TokenString name, TokenComma:xs]
			| name == d.gcd_name			= case fx xs of
				(Just x, [TokenBracketClose:ys])	= (Just (CONS x),ys)
				_									= (Nothing, l)
			| otherwise								= (Nothing, l)
		_									= (Nothing, l)

JSONDecode{|CONS|} fx l = (Nothing, l)
										
JSONDecode{|FIELD of d|} fx l =: [TokenString name, TokenColon : value]
	| d.gfd_name == name	= case fx value of
		(Just x, xs)		= (Just (FIELD x),xs)
		(Nothing,_)			= (Nothing, l)
	| otherwise				= (Nothing, l)
	
JSONDecode{|FIELD|} fx l = (Nothing, l)

JSONDecode{|[]|} fx l = case l of
	[TokenBracketOpen,TokenBracketClose: xs] = (Just [], xs)
	[TokenBracketOpen: xs] = case decodeItems fx xs of
		(Just items, ys)	= (Just items, ys)
		_					= (Nothing, l)
	_						= (Nothing, l)

JSONDecode{|(,)|} fx fy l = case l of
	[TokenBracketOpen:xs] = case fx xs of
		(Just x, [TokenComma:ys]) = case fy ys of
			(Just y, [TokenBracketClose: zs])	= (Just (x,y), zs)
			_									= (Nothing, l)
		_										= (Nothing, l)
	_											= (Nothing, l)

	
JSONDecode{|{}|} fx l = case l of
	[TokenBracketOpen,TokenBracketClose: xs] = (Just {}, xs)
	[TokenBracketOpen: xs] = case decodeItems fx xs of
		(Just items, ys)	= (Just {e \\ e <- items}, ys)
		_					= (Nothing, l)
	_						= (Nothing, l)
	
JSONDecode{|{!}|} fx l = case l of
	[TokenBracketOpen,TokenBracketClose: xs] = (Just {}, xs)
	[TokenBracketOpen: xs] = case decodeItems fx xs of
		(Just items, ys)	= (Just {e \\ e <- items}, ys)
		_					= (Nothing, l)
	_						= (Nothing, l)


decodeItems fx l = case fx l of
	(Just x,[TokenComma:l`]) = case decodeItems fx l` of
		(Just xs, l``)				= (Just [x:xs], l``)
		_							= (Nothing, l)
	(Just x,[TokenBracketClose:xs])	= (Just [x], xs)
	_								= (Nothing, l)

JSONDecode{|Maybe|} fx [TokenNull:xs]	= (Just Nothing, xs)
JSONDecode{|Maybe|} fx l = case fx l of
	(Just x,xs)							= (Just (Just x), xs)
	_									= (Nothing,l)


