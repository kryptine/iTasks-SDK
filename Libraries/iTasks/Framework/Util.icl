implementation module Util

import StdBool, StdArray, StdOverloaded, StdList, StdTuple, StdMisc
import Time
import TSt

import dynamic_string, graph_to_string_with_descriptors, graph_to_sapl_string

derive gPrint Maybe, Void, (,), (,,), (,,,), (,,,,)
derive gParse Maybe, Void, (,), (,,), (,,,), (,,,,)

iTaskId :: !TaskNr !String -> String
iTaskId tasknr postfix 
	# postfix	=	{ c \\ c <-: postfix | not (isMember c ['\\\"/:*?<>|"']) }		// throw away characters not allowed in a file name
	| postfix == ""		= "iTask_" +++ (taskNrToString tasknr) 
	| otherwise			= "iTask_" +++ (taskNrToString tasknr) +++ "-" +++ postfix


(+++>) infixr 5	:: !a !String -> String | gVisualize{|*|} a
(+++>) a s = visualizeAsTextLabel a +++ s

(<+++) infixl 5	:: !String !a -> String | gVisualize{|*|} a
(<+++) s a = s +++ visualizeAsTextLabel a

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


gVisualize{|Task|} fx (Task label _ _) _ vst = ([TextFragment label],vst)

gUpdate{|Task|} fx _ ust=:{mode=UDCreate}
	# (a,ust) = fx undef ust
	= (Task "return" Nothing (\tst -> (a,tst)), ust)
gUpdate{|Task|} fx x ust = (x,ust)


