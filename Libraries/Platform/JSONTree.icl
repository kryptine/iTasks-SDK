implementation module JSONTree

import StdEnv, StdMaybe
import JSON

//--- Decode ----------------------------------
toJSONTree :: String -> Maybe JsonNode
toJSONTree input
# p = (removeWhitespace (snd (lex input 0 [])))
= (parser p)

where
	toArray t = {i \\ i <- t}

	removeWhitespace l = filter (not o isWhitespaceToken) l

	isWhitespaceToken (TokenWhitespace _)	= True
	isWhitespaceToken _						= False

parser :: [Token] -> Maybe JsonNode
parser []		  = Nothing
parser [t:ts] 
| isBracketOpen t = buildArrayNode ts "root"
| isBraceOpen t   = buildObjectNode ts "root"
| otherwise = Nothing
where
	isBracketOpen TokenBracketOpen = True
	isBracketOpen _				   = False
	
	isBraceOpen TokenBraceOpen = True
	isBraceClose _			   = False

buildObjectNode :: [Token] String -> Maybe JsonNode
buildObjectNode [TokenBraceClose] name = Just (JsonEmpty name)
buildObjectNode [t]				  name = Nothing
buildObjectNode input			  name
= case last input of
	TokenBraceClose
		# split = splitContents ( init input )
		# cont  = [ buildNamedNode t \\ t <- split]
		| check cont = Just (JsonObject name [(fromJust t) \\ t <- cont])
		| otherwise = Nothing	
	_				= Nothing
where
	check [] = True
	check [Nothing : ts] = False
	check [(Just obj) : ts] = check ts

buildArrayNode :: [Token] String -> Maybe JsonNode
buildArrayNode [TokenBracketClose] name = Just (JsonEmpty name)
buildArrayNode [t]  			   name = Nothing
buildArrayNode input			   name
= case last input of
	TokenBracketClose
		# split = splitContents ( init input )
		# cont  = [ buildAnonNode t i \\ t <- split & i <- [1..] ]
		| check cont = Just (JsonArray name [(fromJust t) \\ t <- cont])
		| otherwise = Nothing	
	_				= Nothing
where
	check [] = True
	check [Nothing : ts] = False
	check [(Just obj) : ts] = check ts

buildNamedNode :: [Token] -> Maybe JsonNode
buildNamedNode [(TokenString name):TokenColon:rest] = buildNode rest name
buildNamedNode input = Nothing

buildAnonNode :: [Token] Int -> Maybe JsonNode
buildAnonNode input number = buildNode input (toString number)

buildNode :: [Token] String -> Maybe JsonNode
buildNode [(TokenInt v):r] name = Just (JsonInt name v)
buildNode [(TokenReal v):r] name = Just (JsonReal name v)
buildNode [(TokenString v):r] name = Just (JsonString name v) 
buildNode [(TokenBool v):r] name = Just (JsonBool name v)
buildNode [(TokenNull):r] name = Just (JsonNull name)
buildNode [(TokenBracketOpen):r] name = buildArrayNode r name
buildNode [(TokenBraceOpen):r] name = buildObjectNode r name
buildNode r name = Nothing

splitContents :: [Token] -> [[Token]]
splitContents [] = []
splitContents input
	# (c,r) = findComma input [] 0
	= [c : splitContents r]
where
	findComma :: [Token] [Token] Int -> ([Token],[Token])
	findComma [] acc lvl = (acc,[])
	findComma [t:ts] acc lvl
	# lvl						 = adjustLvl t lvl
	| isTokenComma t && lvl == 0 = (acc,ts)
	| otherwise					 = findComma ts (acc++[t]) lvl

	isTokenComma TokenComma = True
	isTokenComma _			= False
	
	adjustLvl TokenBraceOpen    lvl = (lvl+1)
	adjustLvl TokenBracketOpen  lvl = (lvl+1)
	adjustLvl TokenBraceClose   lvl = (lvl-1)
	adjustLvl TokenBracketClose lvl = (lvl-1)
	adjustLvl _					lvl = lvl

//--- Encode ----------------------------------
fromJSONTree :: JsonNode -> String
fromJSONTree (JsonObject name val) = "{"+++jsonTreeToString val+++"}"
fromJSONTree (JsonArray  name val) = "["+++jsonTreeToStringAnon val+++"]"
fromJSONTree node                  = "{"+++jsonTreeToString node+++"}"

class jsonTreeToString a :: a -> String

instance jsonTreeToString JsonNode
where
	jsonTreeToString :: JsonNode -> String
	jsonTreeToString (JsonInt name val) = "\""+++name+++"\" : "+++(toString val)
	jsonTreeToString (JsonReal name val) = "\""+++name+++"\" : "+++(toString val)
	jsonTreeToString (JsonBool name val) = "\""+++name+++"\" : "+++(toString val)
	jsonTreeToString (JsonString name val) = "\""+++name+++"\" : \""+++(toString val)+++"\""
	jsonTreeToString (JsonNull name) = "\""+++name+++"\" : null"
	jsonTreeToString (JsonEmpty name ) = "\""+++name+++"\" : null"
	jsonTreeToString (JsonObject name val) = "\""+++name+++"\" : {"+++jsonTreeToString val+++"}"
	jsonTreeToString (JsonArray name val) = "\""+++name+++"\" : ["+++jsonTreeToStringAnon val+++"]"

instance jsonTreeToString [JsonNode]
where
	jsonTreeToString :: [JsonNode] -> String
	jsonTreeToString []     = ""
	jsonTreeToString [n]    = jsonTreeToString n
	jsonTreeToString [n:ns] = jsonTreeToString n +++ "," +++ jsonTreeToString ns
	
class jsonTreeToStringAnon a :: a -> String

instance jsonTreeToStringAnon JsonNode
where
	jsonTreeToStringAnon :: JsonNode -> String
	jsonTreeToStringAnon (JsonInt name val) = (toString val)
	jsonTreeToStringAnon (JsonReal name val) = (toString val)
	jsonTreeToStringAnon (JsonBool name val) = (toString val)
	jsonTreeToStringAnon (JsonString name val) = "\""+++(toString val)+++"\""
	jsonTreeToStringAnon (JsonNull name) = "null"
	jsonTreeToStringAnon (JsonEmpty name ) = "null"
	jsonTreeToStringAnon (JsonObject name val) = "{"+++jsonTreeToString val+++"}"
	jsonTreeToStringAnon (JsonArray name val) = "["+++jsonTreeToStringAnon val+++"]"

instance jsonTreeToStringAnon [JsonNode]
where
	jsonTreeToStringAnon :: [JsonNode] -> String
	jsonTreeToStringAnon []     = ""
	jsonTreeToStringAnon [n]    = jsonTreeToStringAnon n
	jsonTreeToStringAnon [n:ns] = jsonTreeToStringAnon n +++ "," +++ jsonTreeToStringAnon ns
	

//--- Query -----------------------------------
queryJSONTree :: String JsonNode -> Maybe a | getValue a
queryJSONTree query tree
	# path = splitQuery query
	= seekJsonTree path tree
where
	splitQuery :: {#Char} -> [String]
	splitQuery "" = []
	splitQuery query
		# (c,r) = findSlash query ""
		= [c : splitQuery r]
	
	findSlash :: {#Char} {#Char} -> ({#Char},{#Char})
	findSlash "" acc = (acc,"")
	findSlash query acc
	| query.[0] == '\\' = (acc,{query.[i] \\ i <- [1..((size query)-1)]})
	| otherwise = findSlash {query.[i] \\ i <- [1..((size query)-1)]} (acc+++(toString query.[0]))

seekJsonTree :: [String] JsonNode -> Maybe a | getValue a
seekJsonTree []     node = Nothing
seekJsonTree [t]    node = 
	case seekChild t node of
	(Just child) = getValue child
	Nothing 	 = Nothing
seekJsonTree [t:ts] node =
	case seekChild t node of
	(Just child) = seekJsonTree ts child
	Nothing		 = Nothing

seekChild :: String JsonNode -> Maybe JsonNode
seekChild query (JsonObject name children) = seekChild` query children 
seekChild query (JsonArray name children)  = seekChild` query children 
seekChild query _						   = Nothing

seekChild` :: String [JsonNode] -> Maybe JsonNode
seekChild` query children
	# result = [ child \\ child <- children | (matchName child query) ]
	| length result > 0 = Just (hd result)
						= Nothing
where		
	matchName (JsonInt name val) query
	| (query == name) = True
	                  = False
	matchName (JsonReal name val) query 
	| (query == name) = True
	                  = False
	matchName (JsonString name val) query
	| (query == name) = True
	                  = False
	matchName (JsonBool name val) query 
	| (query == name) = True
	                  = False
	matchName (JsonNull name) query
	| (query == name) = True
	                  = False
	matchName (JsonObject name val) query 
	| (query == name) = True
	                  = False
	matchName (JsonArray name val) query
	| (query == name) = True
	                  = False
	matchName (JsonEmpty name) query 
	| (query == name) = True
	                  = False

instance getValue Int
where
	getValue :: JsonNode -> Maybe Int
	getValue (JsonInt name val) = Just val
	getValue _					= Nothing

instance getValue Real
where
	getValue :: JsonNode -> Maybe Real
	getValue (JsonReal name val) = Just val
	getValue _ 				     = Nothing

instance getValue Bool
where
	getValue :: JsonNode -> Maybe Bool
	getValue (JsonBool name val) = Just val
	getValue _ 				     = Nothing

instance getValue String
where
	getValue :: JsonNode -> Maybe String
	getValue (JsonString name val) = Just val
	getValue _ 				       = Nothing

instance getValue JsonNode
where 
	getValue :: JsonNode -> Maybe JsonNode
	getValue node = Just node

instance getValue [JsonNode]
where 
	getValue :: JsonNode -> Maybe [JsonNode]
	getValue (JsonObject name val) = Just val
	getValue (JsonArray  name val) = Just val
	getValue _					   = Nothing