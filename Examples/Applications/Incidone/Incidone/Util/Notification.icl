implementation module Incidone.Util.Notification
import iTasks
import iTasks.Extensions.DateTime
import Text, System.Time
import Incidone.Util.TaskPatterns

//Notifications are stored newest first
notifications :: SimpleSDSLens [(DateTime,String)]
notifications = sharedStore "notifications" []

//Only show notifications added in the last 5 seconds
currentNotifications :: SDSLens () [String] ()
currentNotifications = mapRead prj (currentDateTime |*| notifications)
where
    prj (now,notifications) = [toString dt +++ msg \\ (dt,msg) <- notifications | limit now dt ]
	limit t1 t2 = False //FIXME: We need an non-pure function to convert the datetime values, we can't do that with a mapRead...
/*
		# (Timestamp s1) = datetimeToTimestamp t1
		# (Timestamp s2) = datetimeToTimestamp t2
		= s1 - s2 < 3
*/

addNotification :: String -> Task ()
addNotification msg
    =   get currentDateTime
    >>- \now ->
        upd (\list -> take 10 [(now,msg):list]) notifications @! ()

viewNotifications :: Task ()
viewNotifications = forever (
    whileUnchanged currentNotifications
        \notifications -> case notifications of
            []      = viewInformation [] () <<@ NoUserInterface
            list    = (Title "Alert" @>> viewInformation [] list @! ()) <<@ InNotificationBubble
    )
