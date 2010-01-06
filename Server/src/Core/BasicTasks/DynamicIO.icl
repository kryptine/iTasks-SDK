implementation module DynamicIO

import StdDynamic, StdDynamicFileIO
import StdFile
import StdMisc

import iTasks

writeDynamicTask :: !String !(Task a) -> Task Bool | iData a
writeDynamicTask filename task 
	= appWorld filename (writeDynamic filename (dynamic task))

readDynamicTask :: !String -> Task (Bool,Task a) | iData a
readDynamicTask filename  
	= 					appWorld filename (readDynamic` filename)
		>>= \(b,d) ->	if b
						(case d of
							(t::Task a^) 	-> return (True,t)
							_				-> return (False,return (abort ("Dynamic read from file " +++ filename +++ " could not be converted to wanted type"))) 
						)
						(return (False,return (abort ("Could not read from file " +++ filename))))
where
	readDynamic` s f 
		# (b,d,f) =  readDynamic s f
		= ((b,d),f)

