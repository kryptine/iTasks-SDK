implementation module Util

import StdBool, StdArray, StdOverloaded, StdList, StdTuple, StdMisc, StdFile
import Time, Text, Base64
import TSt, Types
import CommonDomain

import dynamic_string, graph_to_string_with_descriptors, graph_to_sapl_string

derive JSONEncode	Void, Either
derive JSONDecode	Void, Either

derive bimap		Maybe, (,)

instance iTaskId TaskNr
where
	iTaskId :: !TaskNr !String -> String
	iTaskId tasknr postfix 
		# postfix	=	{ c \\ c <-: postfix | not (isMember c ['\\\"/:*?<>|"']) }		// throw away characters not allowed in a file name
		| postfix == ""		= "iTask_" +++ (taskNrToString tasknr) 
		| otherwise			= "iTask_" +++ (taskNrToString tasknr) +++ "-" +++ postfix

instance iTaskId TaskId
where
	iTaskId :: !TaskId !String -> String
	iTaskId taskid postfix
		# postfix	=	{ c \\ c <-: postfix | not (isMember c ['\\\"/:*?<>|"']) }		// throw away characters not allowed in a file name
		| postfix == ""		= "iTask_" +++ taskid 
		| otherwise			= "iTask_" +++ taskid +++ "-" +++ postfix

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
fileExtension :: !String -> String
fileExtension filename = case (split "." filename) of
	[_]		= ""
	parts	= last parts
	
baseName :: !String -> String
baseName path = last (split "\\" path)

// ******************************************************************************************************

mb2list	:: !(Maybe [a]) -> [a]
mb2list	Nothing = []
mb2list (Just a) = a

list2mb	:: ![a] -> (Maybe [a])
list2mb [] = Nothing
list2mb a = (Just a)

// ******************************************************************************************************


derive gVisualize	Timestamp
derive gUpdate		Timestamp
derive gVerify		Timestamp

JSONEncode{|Dynamic|} dyn					= [JSONString (base64Encode (dynamic_to_string dyn))]

JSONDecode{|Dynamic|} [JSONString string:c]	= (Just (string_to_dynamic {s` \\ s` <-: base64Decode string}), c)
JSONDecode{|Dynamic|} c						= (Nothing, c)

JSONEncode{|Timestamp|} (Timestamp t)		= [JSONInt t]
JSONDecode{|Timestamp|} [JSONInt t:c]		= (Just (Timestamp t), c)
JSONDecode{|Timestamp|} c					= (Nothing, c)
