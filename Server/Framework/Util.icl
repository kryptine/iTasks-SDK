implementation module Util

import StdList, StdMaybe, StdFile, StdMisc, StdArray, Time, Text
import GenVisualize
from Types	import :: TaskNr, :: TaskId, :: Date{..}, :: Time{..}, :: DateTime(..)
from TSt	import taskNrToString

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

mapSt :: (a *st -> (b,*st)) [a] *st -> ([b],*st)
mapSt f [] st = ([], st)
mapSt f [x:xs] st
	# (y, st) = f x st
	# (ys, st) = mapSt f xs st
	= ([y:ys], st)

mb2list	:: !(Maybe [a]) -> [a]
mb2list	Nothing = []
mb2list (Just a) = a

list2mb	:: ![a] -> (Maybe [a])
list2mb [] = Nothing
list2mb a = (Just a)

pad :: Int Int -> String
pad len num = (createArray (max 0 (len - size nums)) '0' ) +++ nums
where 
	nums = toString num
	
decFormat :: Int -> String
decFormat x = toString (x / 100) +++ "." +++ pad 2 (x rem 100)

currentTime :: !*World -> (!Time,!*World)
currentTime world
	# (tm,world) = localTime world
	= ({Time|hour = tm.Tm.hour, min = tm.Tm.min, sec= tm.Tm.sec},world)

currentDate :: !*World -> (!Date,!*World)
currentDate world
	# (tm,world) = localTime world
	= ({Date| day = tm.Tm.mday, mon = 1 + tm.Tm.mon, year = 1900 + tm.Tm.year},world)

currentDateTime :: !*World -> (!DateTime,!*World)
currentDateTime world
	# (tm,world)	= localTime world
	# date			= {Date| day = tm.Tm.mday, mon = 1 + tm.Tm.mon, year = 1900 + tm.Tm.year}
	# time			= {Time|hour = tm.Tm.hour, min = tm.Tm.min, sec= tm.Tm.sec}
	= (DateTime date time,world)
