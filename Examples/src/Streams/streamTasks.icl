implementation module streamTasks

import iTasks

import streamUtil

from StdMisc import abort
from StdFunc import o

derive gVisualize	Stream	
derive gUpdate		Stream	
derive gPrint		Stream, Either	
derive gParse		Stream, Either
derive gHint		Stream
derive gError		Stream
derive bimap		(,), Maybe

// ************************

:: Stream a = ES
			| S a (Task (Stream a))

:: StreamFun a b :== (Task (Stream a)) -> Task (Stream b)

// ************************

// utility functions

nChannel name task	= spawnProcess RootUser True (name @>> task)

// specialized version of waitP applying a function on streams

waitPS :: (a (Task (Stream a)) -> Task (Stream b)) (Task (Stream a)) -> Task (Stream b)  | iTask a & iTask b
waitPS fun str = waitP applyFun str
where
	applyFun ES 		= return ES
	applyFun (S a str) 	= fun a str

// ************************

// stream bind

(|>) infixl 9 :: a (a -> b) -> b
(|>) f g = g f

// stream return

returnS :: String (Stream a) -> Task (Stream a)  | iTask a 
returnS name ES = 		return ES				// end of stream
returnS name (S a str) 							// item in stream
	=					spawnP myname str		// create channel process to fetch next item from stream
		>>= \nstr -> 	return (S a nstr)		// return
where
	myname = name +++ " " +++ printToString a	// for debugging	


// ************************

// generator

generator :: [a] -> Task (Stream a) | iTask a
generator [] 		= return ES
generator [x:xs]	= return (S x (generator xs))	// generates items lazy

// sink

sink :: (Task (Stream a)) -> Task [a] | iTask a
sink str = waitP (show []) str
where
	show accu ES 			
	= 			let result = reverse accu in 
				showMessageAbout "Stream ended: " result
		>>| 	return result
	show accu (S a str) 	
	= 					spawnP myname str			// eager ask for next item...
		>>= \nstr ->	showMessageAbout "Sink received: " a 
		>>| 			waitP (show [a:accu]) nstr 
	where
		myname = "Sink " +++ printToString a	


// ************************

// filters, grouping, ungrouping

filterS :: (a -> Bool) -> StreamFun a a  | iTask a 
filterS pred = waitPS filterS`
where
	filterS` a str 
	| pred a			=	returnS "filter" (S a (filterS pred str))
	= filterS pred str

toList :: Int -> StreamFun a [a]   | iTask a 
toList m 
| m <= 0 	= toList 1				// at least one element required...
|otherwise 	= waitP (take [] m)
where
	take as _ ES		= return (S (reverse as) (return ES))
	take as 1 (S a st) 	= returnS "toList" (S (reverse [a:as]) (toList m st))
	take as n (S a st)	= st >>= take [a:as] (n-1)

fromList :: StreamFun [a] a   | iTask a 
fromList = waitPS fromList2
where
	fromList2 [] st		= fromList st
	fromList2 [a:as] st = returnS "fromList" (S a (fromList2 as st))

// ************************

// sequential map

mapS :: [a -> Task b] -> StreamFun a b  | iTask a & iTask b
mapS [ta:tas] = waitPS mapS`
where
	mapS` a st 
	= 				ta a 
		>>= \b -> 	returnS "mapS" (S b (mapS (tas++[ta]) st))

// parallel map

mapP :: [a -> Task b] -> StreamFun a [b]  | iTask a & iTask b
mapP tasks  
 = 	\st ->					waitP (mapP2 tasks) st
		>>= \(refs,st) -> 	waitForAll refs
		>>= \bs ->			returnS "mapP" (S bs (mapP tasks st))
where
	mapP2 :: [a -> Task b] (Stream a) -> Task ([ProcessRef b], Task (Stream a))  | iTask a & iTask b
	mapP2 _  	 	ES 		= return ([],return ES)
	mapP2 []      	s  		= return ([],return s)
	mapP2 [ta:tas] (S a st) 
		= 						nChannel "mapP" (ta a) 
			>>= \ref -> 		st
			>>=	\s ->			mapP2 tas s
			>>= \(refs,st) -> 	return ([ref:refs],st)

	waitForAll:: [ProcessRef b] -> Task [b]| iTask b 
	waitForAll [] 		= return []
	waitForAll [rb:rbs] = waitForProcess rb >>= handle rbs

	handle rbs Nothing	= waitForAll rbs  	 
	handle rbs (Just b)	= waitForAll rbs >>= \bs -> return [b:bs] 	 
		
dupP :: [a -> Task b] -> StreamFun a [b]  | iTask a & iTask b
dupP tasks = \st -> mapP tasks (dupP` (length tasks) st)
where
	dupP` n st = waitPS  (repeatS n) st
	where
		repeatS 0 a st	= dupP` n st
		repeatS n a st  = return (S a (repeatS (n-1) a st))


splitS :: (a -> Bool) (StreamFun a b) (StreamFun a c) 
				(Task (Stream a)) -> Task (Task (Stream b),Task (Stream c)) | iTask a & iTask b & iTask c
splitS pred okfun notokfun st = st >>= splitS2 pred >>= \(tsaok,tsanok) -> return (okfun tsaok, notokfun tsanok)
where
	splitS2 :: (a -> Bool) (Stream a) -> Task (Task (Stream a),Task (Stream a)) | iTask a
	splitS2 pred ES			= return (return ES, return ES)
	splitS2 pred (S a st)	
	| pred a			= 				nChannel "splitS" (st >>= splitS2 pred)
							>>= \ref -> return (return (S a (fetch fst ref)), fetch snd ref)
	| otherwise			= 				nChannel "splitS" (st >>= splitS2 pred)
							>>= \ref -> return (fetch fst ref,                return (S a (fetch snd ref)))
	where
		fetch fun ref = waitForProcess ref >>= \mbst ->  case mbst of
															(Just val) -> fun val
															_ -> return ES

joinS :: (Task (Task (Stream a),Task (Stream b))) -> Task (Stream (Either a b)) | iTask a & iTask b
joinS st =					st 
			>>= \(sa,sb) -> nChannel "join Left"  sa
			>>= \refsa ->	nChannel "join Right" sb
			>>= \refsb ->	twoStreams refsa refsb
where
	twoStreams refsa refsb
		=  					(waitForProcess refsa >>= \mbna -> doLeft (fromJust mbna) refsb) -||-
							(waitForProcess refsb >>= \mbnb -> doRight refsa (fromJust mbnb))

	doLeft ES refsb			
		= 					waitForProcess refsb 
			>>= \mbnb ->	mapS [\a -> return (Right a)] (return (fromJust mbnb))
	doLeft (S a sa) refsb	
		= 					nChannel "doLeft" sa
			>>= \refsa ->	return (S (Left a)  (twoStreams refsa refsb))

	doRight refsa ES		
		= 					waitForProcess refsa 
			>>= \mbna ->	mapS [\a -> return (Left a)] (return (fromJust mbna))
	doRight refsa (S b sb)			
		= 					nChannel "doRight" sb
			>>= \refsb ->	return (S (Right b)  (twoStreams refsa refsb))

:: DynPipe a = DP (a -> (StreamFun a a, Maybe a, Maybe (DynPipe a)))

pipeline :: (DynPipe a) -> (StreamFun a a)  | iTask a & toString a
pipeline (DP pipefun) = waitPS pipeline2
where
	pipeline2 a st	  
	# (sfun, mba, mbpipefun)= pipefun a
	# nst					= sfun st
	| isNothing mbpipefun
		| isNothing mba		= nst
		= returnS ("last pipe " +++ toString a) (S (fromJust mba) nst)
	| isNothing mba			= pipeline (fromJust mbpipefun) nst
	= returnS ("pipe " +++ toString a) (S (fromJust mba) (pipeline (fromJust mbpipefun) nst))


/*
:: DynPipe a = DP ([(Task (Stream a)) -> Task (Stream a)] a -> ([(Task (Stream a)) -> Task (Stream a)], Maybe a, Maybe (DynPipe a)))

pipeline :: [(Task (Stream a)) -> Task (Stream a)] (DynPipe a) (Task (Stream a)) -> Task (Stream a)  | iTask a & toString a
pipeline funs (DP pipefun) st = waitPS pipeline2 (applyBoxes funs st)
where
	applyBoxes [] st 			= st
	applyBoxes [fun:funs] st	= applyBoxes funs (fun st)

	pipeline2 a st	  
	# (nfuns, mba, mbpipe)	= pipefun funs a
	| isNothing mbpipe 		= st
	| isNothing mba			= pipeline nfuns (fromJust mbpipe) st
	= returnS ("pipeline " +++ toString a) (S (fromJust mba) (pipeline nfuns (fromJust mbpipe) st))

*/

// spawn an asyncronous process

/*
spawnP :: String (Task a) -> Task (Task a) | iTask a					// spawn process, return task fetching its result
spawnP name ta	
	= 					spawnProcess RootUser True (name @>> ta)		// spawn of asynchronous process
		>>= \pid ->		return (waitFor name pid)						// return task which waits for its result upon evaluation
where
	waitFor :: String (ProcessRef a) -> Task a | iTask a
	waitFor name pid 
	=					prompt name 									// prompt that we are waiting 
						||- 
						waitForProcess pid 								// wait for process to complete
		>>= \mbVal -> 	deleteProcess pid								// delete from process table
		>>|				return (fromJust mbVal)							// and return its value
	
	prompt name			= showStickyMessage ("Waiting for process " +++ name) 

// wait for such a process

waitP :: (a -> Task  b) (Task a) -> Task b  | iTask a & iTask b
waitP fun str = str >>= fun

*/


