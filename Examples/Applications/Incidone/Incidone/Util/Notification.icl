implementation module Incidone.Util.Notification
import iTasks
from iTasks._Framework.Util import datetimeToTimestamp
import Text
import Incidone.Util.TaskPatterns

//Notifications are stored newest first 
notifications :: Shared [(DateTime,String)]
notifications = sharedStore "notifications" []

//Only show notifications added in the last 5 seconds
currentNotifications :: ReadOnlyShared [String]
currentNotifications = mapRead prj (currentDateTime |*| notifications)
where
    prj (now,notifications) = [toString dt +++ msg \\ (dt,msg) <- notifications | limit now dt ]
	limit t1 t2
		# (Timestamp s1) = datetimeToTimestamp t1
		# (Timestamp s2) = datetimeToTimestamp t2
		= s1 - s2 < 3

addNotification :: String -> Task ()
addNotification msg
    =   get currentDateTime
    >>- \now ->
        upd (\list -> take 10 [(now,msg):list]) notifications @! ()

viewNotifications :: Task ()
viewNotifications = forever (
    whileUnchanged currentNotifications
        \notifications -> case notifications of
            []      = viewInformation () [] () <<@ NoUserInterface
            list    = (viewInformation (Title "Alert") [] list @! ()) <<@ InNotificationBubble
    )
