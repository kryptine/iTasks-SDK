implementation module streamTasks

import iTasks
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

nProc name task = spawnProcess here True (name @>> task)
where
	here = UserName "root" "Root"

// utilities to apply functions to a stream 

apply :: String ((Stream a) -> Task b)  (Task (Stream a)) -> Task b | iTask a & iTask b
apply  name fun st = name @>> (prompt name  ||- st) >>= fun

applyS :: String (a (Task (Stream a)) -> Task (Stream b)) (Task (Stream a)) -> Task (Stream b)  | iTask a & iTask b
applyS name fun st = name @>> (prompt name  ||- st) >>= apply
where
	apply ES 		= return ES
	apply (S a st) 	= fun a st

prompt name 
	= 					showStickyMessage (name +++ " is waiting for next item in stream...") 

// ************************

// pipeline bind

(|>) infixl 9 :: a (a -> b) -> b
(|>) f g = g f

// pipeline return

returnS :: String (Stream a) -> Task (Stream a)  | iTask a 
returnS name ES 		// end of stream
= return ES
returnS name (S a st) 	// item in stream
=						nProc name st					// start process to collect next item
		>>= \refst -> 	return (S a (nextItem refst))	// return
where
	nextItem refst 
	=					prompt name 					// show process is waiting 
						||- 
						waitForProcess refst 			// wait for next item in stream
		>>= \mbst -> 	return (fromJust mbst)			// and return it


// ************************

// generator

generator :: [a] -> Task (Stream a) | iTask a
generator [] 		= return ES
generator [x:xs]	= return (S x (generator xs))		// generates lazily !

// sink

sink :: (Task (Stream a)) -> Task [a] | iTask a
sink st 
	= 	  apply "Sink" (show []) st						// evaluates eagerly
where
	show as ES 			= let 	result = reverse as in 
											showMessageAbout "Stream ended: " result
										>>| return result
	show as (S a st) 	= 		showMessageAbout "Sink received: " a 
							>>| st >>= show [a:as] 

// ************************

filterS :: (a -> Bool) -> StreamFun a a  | iTask a 
filterS pred = applyS "filter" filterS`
where
	filterS` a st 
	| pred a			=	return (S a (filterS pred st)) 
	= filterS pred st

mapS :: [a -> Task b] -> StreamFun a b  | iTask a & iTask b
mapS [task:tasks] = applyS "mapS" mapS`
where
	mapS` a st 
	= 				task a 
//		>>= \b -> 	return (S b (mapS (tasks++[task]) st))
		>>= \b -> 	returnS "mapS" (S b (mapS (tasks++[task]) st))

mapP :: [a -> Task b] -> StreamFun a [b]  | iTask a & iTask b
mapP tasks  
 = \st -> 					st 
		>>= 				mapP2 tasks 
		>>= \(refs,st) -> 	waitForAll refs
		>>= \bs ->			returnS "mapP" (S bs (mapP tasks st))
where
	mapP2 :: [a -> Task b] (Stream a) -> Task ([ProcessRef b], Task (Stream a))  | iTask a & iTask b
	mapP2 _  	 	ES 		= return ([],return ES)
	mapP2 []      	s  		= return ([],return s)
	mapP2 [task:tasks] (S a st) 
		= 						nProc "mapP" (task a) 
			>>= \ref -> 		st
			>>=	\s ->			mapP2 tasks s
			>>= \(refs,st) -> 	return ([ref:refs],st)

	waitForAll:: [ProcessRef b] -> Task [b]| iTask b 
	waitForAll [] 		= return []
	waitForAll [rb:rbs] = waitForProcess rb >>= handle rbs

	handle rbs Nothing	= waitForAll rbs  	 
	handle rbs (Just b)	= waitForAll rbs >>= \bs -> return [b:bs] 	 
		
dupP :: [a -> Task b] -> StreamFun a [b]  | iTask a & iTask b
dupP tasks = \st -> mapP tasks (dupP` (length tasks) st)
where
	dupP` n st = applyS "dup" (repeatS n) st
	where
		repeatS 0 a st	= dupP` n st
		repeatS n a st  = return (S a (repeatS (n-1) a st))

toList :: Int -> StreamFun a [a]   | iTask a 
toList m 
| m < 1 = abort "toList called with argument less one\n"
 = \st -> st >>= take [] m
where
	take as _ ES		= return (S (reverse as) (return ES))
	take as 1 (S a st) 	= returnS "toList" (S (reverse [a:as]) (toList m st))
	take as n (S a st)	= st >>= take [a:as] (n-1)

fromList :: StreamFun [a] a   | iTask a 
fromList = applyS "fromList" fromList2
where
	fromList2 [] st		= fromList st
	fromList2 [a:as] st = returnS "fromList" (S a (fromList2 as st))

splitS :: (a -> Bool) (StreamFun a b) (StreamFun a c) 
				(Task (Stream a)) -> Task (Task (Stream b),Task (Stream c)) | iTask a & iTask b & iTask c
splitS pred okfun notokfun st = st >>= splitS2 pred >>= \(tsaok,tsanok) -> return (okfun tsaok, notokfun tsanok)
where
	splitS2 :: (a -> Bool) (Stream a) -> Task (Task (Stream a),Task (Stream a)) | iTask a
	splitS2 pred ES			= return (return ES, return ES)
	splitS2 pred (S a st)	
	| pred a			= 				nProc "splitS" (st >>= splitS2 pred)
							>>= \ref -> return (return (S a (fetch fst ref)), fetch snd ref)
	| otherwise			= 				nProc "splitS" (st >>= splitS2 pred)
							>>= \ref -> return (fetch fst ref,                return (S a (fetch snd ref)))
	where
		fetch fun ref = waitForProcess ref >>= \mbst ->  case mbst of
															(Just val) -> fun val
															_ -> return ES

joinS :: (Task (Task (Stream a),Task (Stream b))) -> Task (Stream (Either a b)) | iTask a & iTask b
joinS st =					st 
			>>= \(sa,sb) -> nProc "join Left"  sa
			>>= \refsa ->	nProc "join Right" sb
			>>= \refsb ->	twoStreams refsa refsb
where
	twoStreams refsa refsb
		=  					(waitForProcess refsa >>= \mbna -> doLeft (fromJust mbna) refsb) -||-
							(waitForProcess refsb >>= \mbnb -> doRight refsa (fromJust mbnb))

	doLeft ES refsb			
		= 					waitForProcess refsb 
			>>= \mbnb ->	mapS [\a -> return (Right a)] (return (fromJust mbnb))
	doLeft (S a sa) refsb	
		= 					nProc "doLeft" sa
			>>= \refsa ->	return (S (Left a)  (twoStreams refsa refsb))

	doRight refsa ES		
		= 					waitForProcess refsa 
			>>= \mbna ->	mapS [\a -> return (Left a)] (return (fromJust mbna))
	doRight refsa (S b sb)			
		= 					nProc "doRight" sb
			>>= \refsb ->	return (S (Right b)  (twoStreams refsa refsb))

:: DynPipe a = DP (a -> (StreamFun a a, Maybe a, Maybe (DynPipe a)))

pipeline :: (DynPipe a) -> (StreamFun a a)  | iTask a & toString a
pipeline (DP pipefun) = applyS "pipeline" pipeline2
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
pipeline funs (DP pipefun) st = applyS pipeline2 (applyBoxes funs st)
where
	applyBoxes [] st 			= st
	applyBoxes [fun:funs] st	= applyBoxes funs (fun st)

	pipeline2 a st	  
	# (nfuns, mba, mbpipe)	= pipefun funs a
	| isNothing mbpipe 		= st
	| isNothing mba			= pipeline nfuns (fromJust mbpipe) st
	= returnS ("pipeline " +++ toString a) (S (fromJust mba) (pipeline nfuns (fromJust mbpipe) st))

*/



