implementation module Util

import StdBool, StdArray, StdOverloaded, StdList, StdTuple, StdMisc, StdFile
import Time
import TSt
import Types
import CommonDomain

import dynamic_string, graph_to_string_with_descriptors, graph_to_sapl_string

derive gPrint Maybe, Void, (,), (,,), (,,,), (,,,,), User
derive gParse Maybe, Void, (,), (,,), (,,,), (,,,,), User


iTaskId :: !TaskNr !String -> String
iTaskId tasknr postfix 
	# postfix	=	{ c \\ c <-: postfix | not (isMember c ['\\\"/:*?<>|"']) }		// throw away characters not allowed in a file name
	| postfix == ""		= "iTask_" +++ (taskNrToString tasknr) 
	| otherwise			= "iTask_" +++ (taskNrToString tasknr) +++ "-" +++ postfix


(+++>) infixr 5	:: !a !String -> String | gVisualize{|*|} a
(+++>) a s = visualizeAsTextLabel a +++ s

(<+++) infixl 5	:: !String !a -> String | gVisualize{|*|} a
(<+++) s a = s +++ visualizeAsTextLabel a

readfile :: !String !*World -> (!String,!*World)
readfile filename world
	# (ok,file,world)	= fopen filename FReadData world
	| ok
		# (content,file)= rec file ""
		# (ok,world)	= fclose file world
		= (content,world)
	| otherwise
		= ("",world)
where		
	rec :: *File String -> (String, *File)
	rec file acc # (string, file) = freads file 100
		| string == "" = (acc, file)
		| otherwise    = rec file (acc +++ string)

writefile :: !String !String !*World -> *World
writefile filename content world
	# (ok,file,world)	= fopen filename FWriteData world
	| not ok			= abort ("Failed to write file: " +++ filename)
	# file				= fwrites content file
	# (ok,world)		= fclose file world
	= world

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


gVisualize{|Task|} fx (VValue (Task desc _ _) _) _ vst = ([TextFragment desc.TaskDescription.title],vst)
gVisualize{|Task|} fx _ _ vst = ([],vst)

gUpdate{|Task|} fx _ ust=:{mode=UDCreate}
	# (a,ust) = fx (abort "Task create with undef") ust
	= (Task {TaskDescription|title = "return", description = Note ""} Nothing (\tst -> (a,tst)), ust)
gUpdate{|Task|} fx x ust = (x,ust)

derive gUpdate User
derive gVisualize User


