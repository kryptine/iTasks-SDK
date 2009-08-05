implementation module Util

import StdEnv
import Time
import TSt

import dynamic_string, graph_to_string_with_descriptors, graph_to_sapl_string

derive gPrint Maybe, Void, (,)
derive gParse Maybe, Void, (,)


iTaskId :: !TaskNr !String -> String
iTaskId tasknr postfix 
	# postfix	=	{ c \\ c <-: postfix | not (isMember c ['\\\"/:*?<>|"']) }		// throw away characters not allowed in a file name
	| postfix == ""		= "iTask_" +++ (taskNrToString tasknr) 
	| otherwise			= "iTask_" +++ (taskNrToString tasknr) +++ "-" +++ postfix


// ******************************************************************************************************
// Task specialization
// ******************************************************************************************************
gPrint{|Task|} ga task ps = ps <<- copy_to_string task

gParse{|Task|} ga expr
	# mbstring = parseString expr
	| isNothing mbstring = Nothing
	= Just (fst(copy_from_string {s` \\ s` <-: fromJust mbstring}))
	where
		parseString :: Expr -> Maybe String
		parseString expr = gParse{|*|} expr

// ******************************************************************************************************

gPrint{|Dynamic|} dyn ps = ps <<- dynamic_to_string dyn

gParse{|Dynamic|} expr
	# mbstring = parseString expr
	| isNothing mbstring = Nothing
	= Just (string_to_dynamic {s` \\ s` <-: fromJust mbstring})
	where
		parseString :: Expr -> Maybe String
		parseString expr = gParse{|*|} expr

