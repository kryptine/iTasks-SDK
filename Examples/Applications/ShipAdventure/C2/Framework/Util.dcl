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

editSharedList :: (Shared sds [a]) -> Task () | iTask a & RWShared sds

editSharedListWithTask :: (a -> Task a) (Shared sds [a]) -> Task () | iTask a & RWShared sds

editSharedListWithTaskTask :: (Task a)  (a -> Task a) (Shared sds [a]) -> Task () | iTask a & RWShared sds

editSharedListGeneric :: [EditSharedListOption a] (Shared sds [a]) -> Task () | iTask a & RWShared sds

updateItemInSharedList :: a (a -> Bool) (Shared sds [a]) -> Task [a] | iTask a & RWShared sds

doOrClose :: (Task a) -> Task (Maybe a) | iTask a

doTaskPeriodicallyUntilPause :: Int (Task a) -> Task () | iTask a

doTaskPeriodically :: Int (Task a) -> Task a | iTask a

//doLoggedIn :: (User ->  Task a)  -> Task a| iTask a

lastElems :: Int [a] -> [a]

showInfo :: String -> Task String

:: ChatMessage = {sender   :: String
                 ,when     :: DateTime
                 ,message  :: String
                 }
derive class iTask ChatMessage

chatDialog :: User [Entity] -> Task ()

editChats :: Task ()

chats ::  SDSLens () [ChatMessage] [ChatMessage]

viewChats :: Int -> Task ()

debugstore :: SDSLens () [String] [String]

addDebug :: String -> Task ()

showDebug :: Task ()

ppLatLng :: !LatLng -> String

ppAngle :: !Angle -> String

ppLat :: !Angle -> String

ppLon :: !Angle -> String

roundToNDec :: !Int !Real -> Real
