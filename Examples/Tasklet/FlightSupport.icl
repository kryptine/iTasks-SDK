implementation module FlightSupport

import StdEnv, Maybe

instance == Seat
where
	(==) (Seat r1 s1) (Seat r2 s2) = r1 == r2 && s1 == s2

instance < Seat
where
	(<) (Seat r1 s1) (Seat r2 s2) = r1 < r2 || s1 < s2

instance fromString Seat
where
 fromString str = Seat (toInt (str % (0,size str - 2))) (fromChar (str.[size str - 1] - 'A'))

instance toString Seat
where
 toString (Seat r s) = toString r +++ toString ('A' + (toChar s))

find :: (a -> Bool) [a] -> Maybe a
find f as = case filter f as of
	[] = Nothing					
	fs = Just (hd fs)