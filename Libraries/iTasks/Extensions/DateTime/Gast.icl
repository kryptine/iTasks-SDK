implementation module iTasks.Extensions.DateTime.Gast

import StdEnv, Gast, Data.List, Data.Functor, iTasks.Extensions.DateTime

// Years can be negative (BC), the range of +/- 3000 years is chosen more or less arbitrarily,
// but should include most years used in realistic programs.
ggen{|Date|} _ =
	[ {Date| year = y, mon = m, day = d}
	\\ (y, m, d) <- diag3 [0, -3000, 3000: [-2999..2999]] [1, 12: [2..11]] [1, 31: [2..30]]
	| isValid y m d
	]
where
	isValid :: !Int !Int !Int -> Bool
	isValid y  2  d = if (isLeapYear y) (d <= 29) (d <= 28)
	isValid _  4 31 = False
	isValid _  6 31 = False
	isValid _  9 31 = False
	isValid _ 11 31 = False
	isValid _  _  _ = True

	isLeapYear :: !Int -> Bool
	isLeapYear year = (year rem 4 == 0 && year rem 100 <> 0) || year rem 400 == 0

ggen{|Time|} _ =
	[{Time| hour = h, min = m, sec = s} \\ (h, m, s) <- diag3 [0, 23: [1..22]] [0, 59: [1..58]] [0, 59, 60: [1..58]]]

ggen{|DateTime|} st = (uncurry toDateTime) <$> ggen{|*|} st

derive genShow Date, Time, DateTime
