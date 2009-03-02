implementation module StdListExt

import StdList

span_inc :: (a -> Bool) [a] -> ([a],[a])
span_inc pred xs
	= case span pred xs of
		(bef,[incl:aft])	= (bef ++ [incl],aft)
		not_incl			= not_incl

append :: a [a] -> [a]
append x xs					= xs ++ [x]

replace :: (a a -> Bool) a [a] -> [a]
replace cond new []			= [new]
replace cond new [x:xs]
	| cond new x			= [new : xs]
	| otherwise				= [x : replace cond new xs]
