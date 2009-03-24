implementation module Text

import StdOverloaded, StdString, StdArray, StdChar, StdInt, StdBool, StdClass, StdList

instance Text String
	where
	textSize :: !String -> Int 
	textSize s = size s	

	split :: !String !String -> [String]
	split sep s
		# index = indexOf sep s
		| index == -1	= [s]
						= [s % (0, index - 1): split sep (s % (index + (size sep), size s))]

	join :: !String ![String] -> String
	join sep [] = ""
	join sep [x:[]] = x
	join sep [x:xs] = x +++ sep +++ (join sep xs)

    indexOf :: !String !String -> Int
	indexOf "" haystack = -1
	indexOf needle haystack = `indexOf needle haystack 0
		where
		`indexOf needle haystack n
			| (n + size needle) > (size haystack)									= -1
			| and [needle.[i] == haystack.[n + i] \\ i <- [0..((size needle) - 1)]]	= n
																					= `indexOf needle haystack (n + 1)

    lastIndexOf :: !String !String -> Int
	lastIndexOf "" haystack = -1
	lastIndexOf needle haystack = `lastIndexOf needle haystack (size haystack)
		where
		`lastIndexOf needle haystack n
			| n < 0																	= -1		
			| and [needle.[i] == haystack.[n + i] \\ i <- [0..((size needle) - 1)]]	= n
																					= `lastIndexOf needle haystack (n - 1)

    startsWith :: !String !String -> Bool
	startsWith needle haystack = indexOf needle haystack == 0

    endsWith :: !String !String -> Bool
	endsWith needle haystack = lastIndexOf needle haystack == (size haystack) - (size needle)

    subString :: !Int !Int !String -> String
	subString start len haystack = haystack % (start, start + len - 1)

	replaceSubString :: !String !String !String -> String
	replaceSubString needle replacement haystack
		| index == -1	= haystack
		| otherwise		= start +++ replacement +++ (replaceSubString needle replacement end)
			where
			index	= indexOf needle haystack
			start	= subString 0 index haystack
			end		= subString (index + size needle) (size haystack) haystack
    trim :: !String -> String
	trim s = ltrim (rtrim s)

	ltrim :: !String -> String
	ltrim ""			= ""
	ltrim s
		| isSpace s.[0] 	= if (size s == 1) "" (ltrim (s % (1, size s - 1)))
							= s

	rtrim :: !String -> String
	rtrim ""					= ""
	rtrim s
		| isSpace s.[size s - 1]	= if (size s == 1) "" (rtrim (s % (0, size s - 2)))
									= s

    toLowerCase :: !String -> String
	toLowerCase s = {toLower c \\ c <-: s}

    toUpperCase :: !String -> String
	toUpperCase s = {toUpper c \\ c <-: s}
