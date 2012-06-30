definition module FlightSupport

import StdString, Maybe

:: Seat = Seat Int Int

instance == Seat
instance < Seat

instance fromString Seat
instance toString Seat

find :: (a -> Bool) [a] -> Maybe a
