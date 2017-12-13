implementation module iTasks.Extensions.Editors.Terminal

from Data.Func import $
import Text.HTML
import Text
from StdFunc import o, id, const, flip
import StdMisc
import StdInt, StdOverloaded, _SystemArray, StdChar, StdClass, StdList, StdBool

import qualified Data.Map as DM

/*
 * View the string as a VT100 terminal
 * @param width
 * @param size
 */
terminalRender :: Int Int String -> HtmlTag
terminalRender w h data
	= TtTag [StyleAttr "background-color: white"] $ render 0 0 0 w h data
where
	render :: Int Int Int Int Int String -> [HtmlTag]
	render i x y w h data
	| i >= size data     = []
	| data.[i] == ' '    = [Html $ "&nbsp;":render (i+1) (x+1) y w h data]
	| data.[i] == '\t'   = [Html $ concat $ repeatn ((x + 1) rem 8) "&nbsp;":render (i+1) (x+1) y w h data]
	| data.[i] == '\n'   = [BrTag []:render (i+1) 0 (y+1) w h data]
//Escapes not supported yet
	| data.[i] == '\x1B' && data.[i+1] == '['
	                     = escape (i+2) data
	| x == w             = render i 0 (y+1) w h data
                         = [Text $ toString data.[i]:render (i+1) (x+1) y w h data]
	where
		escape :: Int String -> [HtmlTag]
		escape i data = case take (not o flip isMember ends) i data of
			args
				# l = size args
				// Color
				| data.[i+l] == 'm' = case split ";" args of
					[f1]       = format f1 "0" "0"  $ render (l+i+1) x y w h data
					[f1,f2]    = format f1 f2 "0"   $ render (l+i+1) x y w h data
					[f1,f2,f3] = format f1 f2 f3    $ render (l+i+1) x y w h data
					[] = abort $ "No arguments"
					_ = abort $ "No more than three arguments supported"
				// Cursor
				//| args.[l] == 'H' =
				// Other
				= render (i + l + 1) x y w h data
//
		take :: (Char -> Bool) Int String -> String
		take f i data = if (f data.[i]) (toString (data.[i]) +++ take f (i+1) data) ""
//
		ends :: [Char]
		ends = ['m', 'K', 'H', 'f']

		format :: String String String [HtmlTag] -> [HtmlTag]
		format fm fg bg rest = [SpanTag [StyleAttr $ concat $ map (maybe "" id) $
			['DM'.get (toInt $ toString fm) formats
			,'DM'.get (toInt $ toString fg) foregrounds
			,'DM'.get (toInt $ toString bg) backgrounds
			]] rest]

		formats :: 'DM'.Map Int String
		formats = 'DM'.fromList
			[(0, "font-weight: normal; color: black; background-color: white;")
			,(1, "font-weight: bold;")           ,(2, "") //Dim
			,(4, "text-decoration: underline;")  ,(5, "") //Blink
			,(7, "") /*Reverse*/                 ,(8, "") //Hidden
			,(21, "font-weight: normal;")        ,(22, "") //Reset dim
			,(24, "text-decoration: none;")      ,(25, "") //Reset blink
			,(27, "") /*Reset reverse*/          ,(28, "") //Reset hidden
			]
		
		foregrounds :: 'DM'.Map Int String
		foregrounds = 'DM'.fromList
			[(39,  "color: black;")      ,(30,  "color: black;")
			,(31,  "color: red;")        ,(32,  "color: green;")
			,(33,  "color: yellow;")     ,(34,  "color: blue;")
			,(35,  "color: magenta;")    ,(36,  "color: cyan;")
			,(37,  "color: lightgray")   ,(90,  "color: darkgray")
			,(91,  "color: lightred")    ,(92,  "color: lightgreen")
			,(93,  "color: lightyellow") ,(94,  "color: lightblue")
			,(95,  "color: lightmagenta"),(96,  "color: lightcyan")
			,(97,  "color: white;")
			]
		
		backgrounds :: 'DM'.Map Int String
		backgrounds = 'DM'.fromList
			[(49,  "background-color: white;")      ,(40,  "background-color: black;")
			,(41,  "background-color: red;")        ,(42,  "background-color: green;")
			,(43,  "background-color: yellow;")     ,(44,  "background-color: blue;")
			,(45,  "background-color: magenta;")    ,(46,  "background-color: cyan;")
			,(47,  "background-color: lightgray")   ,(100, "background-color: darkgray")
			,(101, "background-color: lightred")    ,(102, "background-color: lightgreen")
			,(103, "background-color: lightyellow") ,(104, "background-color: lightblue")
			,(105, "background-color: lightmagenta"),(106, "background-color: lightcyan")
			,(107, "background-color: white;")
			]
