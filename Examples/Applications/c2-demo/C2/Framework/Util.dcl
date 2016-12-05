definition module C2.Framework.Util

import iTasks
import C2.Framework.Entity

/* Data structures */

:: CompartmentNo :== Int
:: EntityName :== String

:: Location
  = OnBoard CompartmentNo
  | AtEntity EntityName

::EditSharedListOption a = ESLAdd (String,Task a)
						|  ESLUpdate (String,a -> Task a)
						| ESLView (String,a -> Task ())
						| ESLDel
						| ESLClearAll  

derive class iTask Location

//editUsers :: Task ()

/* Utility tasks */

editSharedList :: (Shared [a]) -> Task () | iTask a

editSharedListWithTask :: (a -> Task a) (Shared [a]) -> Task () | iTask a

editSharedListWithTaskTask :: (Task a)  (a -> Task a) (Shared [a]) -> Task () | iTask a

editSharedListGeneric :: [EditSharedListOption a] (Shared [a]) -> Task () | iTask a

updateItemInSharedList :: a (a -> Bool) (Shared [a]) -> Task [a] | iTask a

doOrClose :: (Task a) -> Task (Maybe a) | iTask a

doTaskPeriodicallyUntilPause :: Int (Task a) -> Task () | iTask a

doTaskPeriodically :: Int (Task a) -> Task a | iTask a

//doLoggedIn :: (User ->  Task a)  -> Task a| iTask a

lastElems :: Int [a] -> [a]

showInfo :: String -> Task String

:: ChatMessage = {sender   :: String
                 ,when     :: DateTime
                 ,message  :: Note
                 }
derive class iTask ChatMessage

chatDialog :: User [Entity] -> Task ()

editChats :: Task ()

chats ::  Shared [ChatMessage]

viewChats :: Int -> Task ()

debugstore :: Shared [String]

addDebug :: String -> Task ()

showDebug :: Task ()

ppLatLng :: !LatLng -> String

ppAngle :: !Angle -> String

ppLat :: !Angle -> String

ppLon :: !Angle -> String

roundToNDec :: !Int !Real -> Real
