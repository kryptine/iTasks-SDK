definition module JSONTree

/*
* This module provides functions to encode and decode JSON-data without having any prior
* knowledge about the structure of the JSON-data. The data is parsed and a tree of type
* 'JsonNode' is build, which can then be used for further processing. The 'queryJsonNode'
* -function allows for querying the tree structure.
*
* This module depends on the JSON-module for lexing the string input.
*/

import StdMaybe

class getValue a :: JsonNode -> Maybe a

instance getValue Int
instance getValue Real
instance getValue Bool
instance getValue String
instance getValue JsonNode
instance getValue [JsonNode]

//The JsonNode type definition
:: JsonNode		= JsonInt String Int
				| JsonReal String Real
				| JsonString String String
				| JsonBool String Bool
				| JsonNull String
				| JsonObject String [JsonNode]
				| JsonArray String [JsonNode]
				| JsonEmpty String

/*
* Encode a JsonNode-tree into it's string representation
*
* @param   The JsonNode-tree
* @return  The String representation in JSON-format
*/
fromJSONTree :: JsonNode -> String

/*
* Convert a JSON-string into a JsonNode-tree
*
* @param   The Json-string
* @return  The JsonNode-tree
*/
toJSONTree :: String -> Maybe JsonNode

/*
* Seek a specific value in the tree. 
* 
* @param   The query string, consisting of node names (excluding, the 'root' node)
*          seperated by \\ (e.g. 'node 1\\node 2'). Array have integer indexes 
*          assigned to them, e.g. 'node1\\2\\node 2' is a valid expression.
* @return  The value of the node
*/
queryJSONTree :: String JsonNode -> Maybe a | getValue a