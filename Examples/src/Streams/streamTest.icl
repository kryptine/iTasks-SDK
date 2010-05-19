module streamTest

from StdMisc import abort
from StdFunc import o

import iTasks
import streamTasks

Start :: *World -> *World
Start world = startEngine wfl world

wfl :: [Workflow]
wfl
= [	workflow "stream" test00
  ]

edit :: String -> (Int -> Task Int)
edit s = updateInformation s

test0 = 	generator [1..5] 
		|> 	sink
test00 = 	generator [1..2] 
		|> 	mapS 	[updateInformation "verander I"]
		|> 	mapS 	[updateInformation "verander II"]
		|> 	sink
test01 = 	generator [1..10] 
		|> 	toList 3 
		|> 	sink
test011 = 	generator [1..10] 
		|> 	filterS isEven
		|> 	sink
test02 = 	generator [1..10] 
		|> 	toList 3 
		|>	fromList
		|> 	sink
test03 = 	generator [1..3] 
		|> 	mapS 	[updateInformation "verander I"]
		|> 	mapS 	[updateInformation "verander II"]
		|> 	filterS isEven
		|> 	sink
test04 = 	generator [1..10] 
		|> 	mapS [\x -> return (x ^ 2)] 
		|> 	sink
test05 = 	generator [1..5] 
		|>	dupP [\a -> return a, \a -> return a] 
		|>	sink
test06 = 	generator [1..5] 
		|> 	mapS 	[edit "oneven taken"
			   		,edit "even taken"
			   	   	]
	  	|>	sink 
test07 = 	generator [1..5] 
		|>	mapP 	[edit "oneven taken"
					,edit "even taken"
					]
	  	|>	sink
test08 = 	generator [1..5] 
		|>	dupP 	[edit "oneven taken"
					,edit "even taken"
					]
	  	|>	sink
test09 = 	generator [1..4] 
		|> 	mapS 	[edit "verander I"]
		|>	splitS isEven (mapS [edit "Even"]) (mapS [edit "Oneven"]) 
		|>	joinS
	  	|>	sink
	  		 


test11 = 	generator [1..10] 
		|> 	pipeline (DP (fib 1 1))
		|> 	sink
where
	fib n m _ = let nm = n + m in (id,Just nm, Just (DP (fib m (n+m))))

test12 = 	generator [2..100] 
		|> 	pipeline (DP sieve)
		|> 	sink
where
	sieve p
	= (filterS (strike p), Just p, Just (DP sieve))

	strike p a = not (a rem p == 0)
	
