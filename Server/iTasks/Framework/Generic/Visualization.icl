implementation module iTasks.Framework.Generic.Visualization

import StdGeneric, StdList
import Data.Maybe, Data.Either, Data.Void, Data.Map, Data.Functor, Data.List
import Text, Text.JSON, Text.HTML
import System.Time
import iTasks.Framework.Util

visualizeAsLabel :: !a -> String | gVisualizeText{|*|} a
visualizeAsLabel v = concat (gVisualizeText{|*|} AsLabel v)

visualizeAsText :: !a -> String | gVisualizeText{|*|} a
visualizeAsText v = join "\n" (gVisualizeText{|*|} AsText v)

visualizeAsRow :: !a -> [String] | gVisualizeText{|*|} a
visualizeAsRow v = gVisualizeText{|*|} AsRow v
	
//Generic text visualizer
generic gVisualizeText a :: !VisualizationFormat !a -> [String]

gVisualizeText{|UNIT|} _ _ = []

gVisualizeText{|RECORD|} fx mode (RECORD x)
	# viz = fx mode x
	= case mode of
		AsLabel		= take 1 viz
		AsText		= viz
		AsRow		= viz
		
gVisualizeText{|FIELD of {gfd_name}|} fx mode (FIELD x)
	# viz = fx mode x
	= case mode of
		AsText		= [camelCaseToWords gfd_name, ": ": viz] ++ [" "]
		AsLabel		= viz
		AsRow		= viz
		
gVisualizeText{|OBJECT|} fx mode (OBJECT x) = fx mode x

gVisualizeText{|CONS of {gcd_name,gcd_type_def}|} fx mode (CONS x)
	= normalADTStaticViz (fx mode x)
where
	normalADTStaticViz viz
		//If viz is empty, only show constructor name
		| isEmpty viz
			= [gcd_name]
		//If there are multiple constructors, also show the name of the constructor
		| gcd_type_def.gtd_num_conses > 1
			= intersperse " " [gcd_name:viz]
		//Otherwise show visualisation of fields separated by spaces
		| otherwise
			= intersperse " " viz

gVisualizeText{|PAIR|} fx fy mode (PAIR x y) = fx mode x ++ fy mode y

gVisualizeText{|EITHER|} fx fy mode either = case either of
	LEFT x	= fx mode x
	RIGHT y	= fy mode y

gVisualizeText{|Int|}			_ val				= [toString val]
gVisualizeText{|Real|}			_ val				= [toString val]
gVisualizeText{|Char|}			_ val				= [toString val]
gVisualizeText{|String|}		_ val				= [toString val]
gVisualizeText{|Bool|}			_ val				= [toString val]

gVisualizeText {|[]|} fx  mode val					= [concat (["[":  flatten (intersperse [", "] [fx mode x \\ x <- val])] ++ ["]"])]
gVisualizeText{|Maybe|} fx mode val					= fromMaybe ["-"] (fmap (\v -> fx mode v) val)

gVisualizeText{|Void|} _ _					= []
gVisualizeText{|Dynamic|} _ _				= []
gVisualizeText{|(->)|} _ _ _ _				= []
gVisualizeText{|JSONNode|} _ val			= [toString val]
gVisualizeText{|HtmlTag|} _ html			= [toString html]

derive gVisualizeText Either, (,), (,,), (,,,), Timestamp, Map

(+++>) infixr 5	:: !a !String -> String | gVisualizeText{|*|} a
(+++>) a s = visualizeAsLabel a +++ s

(<+++) infixl 5	:: !String !a -> String | gVisualizeText{|*|} a
(<+++) s a = s +++ visualizeAsLabel a
