implementation module StdListExt

import StdList

span_inc :: (a -> Bool) [a] -> ([a],[a])
span_inc pred xs
	= case span pred xs of
		(bef,[incl:aft])	= (bef ++ [incl],aft)
		not_incl			= not_incl

append :: a [a] -> [a]
append x xs					= xs ++ [x]
