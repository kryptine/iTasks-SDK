implementation module Incidone.Util.Notification
import iTasks
import Text
import Incidone.Util.TaskPatterns

//Notifications are stored newest first 
notifications :: Shared [(DateTime,String)]
notifications = sharedStore "notifications" []

//Only show notifications added in the last 5 seconds
currentNotifications :: ReadOnlyShared [String]
currentNotifications = mapRead prj (currentDateTime |+| notifications)
where
    prj (now,notifications) = [msg \\ (dt,msg) <- notifications | now - dt < limit]

    limit = DateTime {Date|day=0,mon=0,year=0} {Time|hour=0,min=0,sec=5}

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
