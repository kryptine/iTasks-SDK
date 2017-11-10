implementation module iTasks.API.Extensions.Distributed._Formatter

import graph_copy_with_names
import symbols_in_program
import _SystemArray
import Text.Encodings.Base64
import iTasks

deserializeFromBase64 :: String !{#Symbol} -> a
deserializeFromBase64 input symbols
	= case json (toString (base64Decode input)) of
		(Just data) 	# (x, y, z) = deserializeFromString data
						= fst (copy_from_string_with_names x y z symbols)

serializeToBase64 :: a -> String
serializeToBase64 item = base64Encode (toString (toJSON (serializeToString (copy_to_string_with_names (item)))))

json :: String -> Maybe [String]
json x = fromJSON (fromString x)

serializeToString :: (String,!*{#DescInfo},!*{#String}) -> [String]
serializeToString (state, descInfo, module) = ["State", base64Encode state : packDescInfo [e \\ e <-: descInfo]] ++ packModule module
where
	packDescInfo :: [DescInfo] -> [String]
	packDescInfo [] = []
	packDescInfo [x:xs] = ["DescInfo", toString (x.di_prefix_arity_and_mod), toString (base64Encode x.di_name) : packDescInfo xs]

	packModule :: {#String} -> [String]
	packModule array = [base64Encode e \\ e <-: array]

deserializeFromString :: [String] -> (!*{#Char},!*{#DescInfo},!*{#String})
deserializeFromString ["State", state : xs] = (base64Decode state, {e \\ e <- desc}, {base64Decode e \\ e <- rest})
where
	(desc, rest) = unpackDescInfo xs

	unpackDescInfo :: [String] -> ([DescInfo], [String])
	unpackDescInfo [] = ([], [])
	unpackDescInfo ["DescInfo", di_prefix_arity_and_mod, di_name : xs] 
		= let (x, rest) = unpackDescInfo xs in ([{di_prefix_arity_and_mod = (toInt di_prefix_arity_and_mod), di_name = base64Decode di_name} : x], rest)
	unpackDescInfo xs = ([], xs)
