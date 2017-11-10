implementation module iTasks.API.Extensions.Distributed._Util

import iTasks
from iTasks._Framework.Store import memoryStore, :: StoreName, :: StoreNamespace
from System.Time import :: Timestamp(..)
from iTasks._Framework.Util import timestampToGmDateTime, datetimeToTimestamp

memoryShare :: String a -> RWShared () a a | iTask a
memoryShare name default = sdsFocus name (memoryStore name (Just default))

repeatClient :: (Task (Maybe a)) -> Task (Maybe a) | iTask a
repeatClient task
	= (try task) <! isJust
where   
	try :: (Task (Maybe a)) -> Task (Maybe a) | iTask a
	try task
		= catchAll task (\_ -> return Nothing)
		>>- \result -> if (isNothing result) tryAgain (return result)
                
	tryAgain :: Task (Maybe a) | iTask a
	tryAgain
		= waitForTimer` timeout @! Nothing
	where   
		timeout = 60
                
waitForTimer` :: !Int -> Task DateTime
waitForTimer` interval = get currentDateTime >>- \now -> waitForDateTime` (endTime interval now)
where
	endTime interval now = let (Timestamp ts) = datetimeToTimestamp now in timestampToGmDateTime (Timestamp (ts + interval))

waitForDateTime` :: !DateTime -> Task DateTime
waitForDateTime` datetime
	= viewSharedInformation ("Connection interrupted", ("The connection with the other controller is interrupted, next attempt: " +++ toString datetime)) [] currentUTCDateTime >>* [OnValue (ifValue (\now -> datetime < now) return)]
	>>* [ OnValue (ifValue (\now -> datetime < now) return)
	    , OnAction (Action "Reconnect") (always (return datetime))
	    ]
