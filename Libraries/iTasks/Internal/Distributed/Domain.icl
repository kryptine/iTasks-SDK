implementation module iTasks.Internal.Distributed.Domain

import iTasks

derive class iTask Domain

instance == Domain where
	(==) (Domain h1 p1) (Domain h2 p2) = h1 == h2 && p1 == p2
