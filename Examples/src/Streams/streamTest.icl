module streamTest

from StdMisc import abort
from StdFunc import o

import iTasks
import streamTasks

Start :: *World -> *World
Start world = startEngine wfl world

wfl :: [Workflow]
wfl
= [	workflow "stream" ("stream test" @>> test30)
  ]

edit :: String -> (Int -> Task Int)
edit s = updateInformation s

// generator, sink, filters

test00 = 	generator [1..5] 
		|> 	sink
test01 = 	generator [1..10] 
		|> 	filterS isEven
		|> 	sink
test02 = 	generator [1..10] 
		|> 	toList 3 
		|> 	sink
test03 = 	generator [1..10] 
		|> 	toList 3 
		|>	fromList
		|> 	sink

// mapS test

test10 = 	generator [1..10] 
		|> 	mapS [\x -> return (x ^ 2)] 
		|> 	sink
test11 = 	generator [1..4] 
		|> 	mapS 	[updateInformation "verander"]
		|> 	sink
test12 = 	generator [1..5] 
		|> 	mapS 	[edit "oneven taken"
			   		,edit "even taken"
			   	   	]
	  	|>	sink 
test13 = 	generator [1..3] 
		|> 	mapS 	[updateInformation "verander I"]
		|> 	filterS isEven
		|> 	mapS 	[updateInformation "verander II"]
		|> 	sink

// mapP test

test20 = 	generator [1..5] 
		|>	mapP 	[edit "oneven taken"
					,edit "even taken"
					]
	  	|>	sink

// dupP test // crashes due to fusion error

test30 = 	generator [1..5] 
		|>	mapFun (repeatn 2)
		|>  fromList
		|>  mapP 	[edit "oneven taken"
					,edit "even taken"
					]
	  	|>	sink


// split & join test // crashes due to fusion error

test40 = 	generator [1..4] 
		|> 	mapS 	[edit "verander I"]
		|>	splitS isEven (mapS [edit "Even"]) (mapS [edit "Oneven"]) 
		|>	joinS
	  	|>	sink
  		 
test41 = 	generator [1..4] 
		|>	splitS isEven id id 
		|>	joinS
	  	|>	sink

// pipeline tests

test50 = 	generator [1..10] 
		|> 	pipeline (DP (fib 1 1))
		|> 	sink
where
	fib n m _ = let nm = n + m in (id, Just nm, Just (DP (fib m (n+m))))

test51 = 	generator [2..40] 
		|> 	pipeline (DP sieve)
		|> 	sink
where
	sieve p
	= (filterS (strike p), Just p, Just (DP sieve))

	strike p a = not (a rem p == 0)
	
