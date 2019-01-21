implementation module C2.Framework.Util

import iTasks
import iTasks.Extensions.DateTime
import C2.Framework.Entity
import Text, Math.Geometry

derive class iTask Location

//editUsers :: Task ()
//editUsers = editSharedList users

//setUsers :: [User] -> Task [User]
//setUsers users = set users users


/* Utility tasks */

editSharedList :: (Shared sds [a]) -> Task () | iTask a & RWShared sds
editSharedList list
	= editSharedListWithTask (updateInformation "Item Info" []) list

editSharedListWithTask :: (a -> Task a) (Shared sds [a]) -> Task () | iTask a & RWShared sds
editSharedListWithTask tupdate list
	= editSharedListWithTaskTask  (enterInformation "Enter new item" []) tupdate list

editSharedListWithTaskTask :: (Task a)  (a -> Task a) (Shared sds [a])-> Task () | iTask a & RWShared sds
editSharedListWithTaskTask tenter tupdate list
	= editSharedListGeneric [ESLUpdate ("Edit Item",tupdate)
							,ESLAdd    ("Add Item",tenter)
							,ESLDel
							,ESLClearAll] list

editSharedListGeneric :: [EditSharedListOption a] (Shared sds [a]) -> Task () | iTask a & RWShared sds
editSharedListGeneric options list
	= doOrClose (forever (enterChoiceWithShared "Choose an item"
    	[ChooseFromGrid snd]
        (mapRead (\ps -> [(i,p) \\ p <- ps & i <- [0..]]) list)
  	>>* [OnAction (Action desc) 				(always (addItem t))
  		\\ (ESLAdd (desc,t)) <- options] 	++
      	[OnAction (Action desc) 				(hasValue (editItem t))
      	\\ (ESLUpdate (desc,t)) <- options] ++
      	[OnAction (Action desc)    			(hasValue (viewItem t))
      	\\ (ESLView (desc,t)) <- options] 	++
      	[OnAction (Action "Delete") 			(hasValue deleteItem)
      	\\ ESLDel <- options]          		++
      	[OnAction (Action "Clear All")  		(always clearAll)
      	\\ ESLClearAll <- options] )) @! ()

where addItem  tenter  = tenter >>= \item -> upd (\us -> us ++ [item]) list @! ()
      deleteItem (k,u) = upd (\us -> removeAt k us) list  @! ()
      editItem t (k,u) =   t u
                       >>= \item -> upd (\us -> updateAt k item us) list
                       @!  ()
      viewItem t (k,u) = t u @! ()
      clearAll         = viewInformation "Clear All" []
      									 "Are you sure you want to delete all items?"
                         >>* [OnAction ActionOk
                         		(always (upd (\us -> []) list @! ()))
                             ,OnAction ActionCancel
                                (always (return ()))
                             ]

doOrClose :: (Task a) -> Task (Maybe a) | iTask a
doOrClose task = ((task @ Just) -||- chooseAction [(ActionClose,Nothing)]) >>- return

updateItemInSharedList :: a (a -> Bool) (Shared sds [a]) -> Task [a] | iTask a & RWShared sds
updateItemInSharedList newitem cond share = upd f share
where f []                 = []
      f [a:as] | cond a    = [newitem : as]
               | otherwise = [a : f as]

doTaskPeriodically :: Int (Task a) -> Task a | iTask a
doTaskPeriodically period task = forever (waitForTimer period >>| task)

doTaskPeriodicallyUntilPause :: Int (Task a) -> Task () | iTask a
doTaskPeriodicallyUntilPause period task
  =   doTaskPeriodically period task
  >>* [OnAction (Action "Pause simulation") (always (return ()))]

//doLoggedIn :: (User ->  Task a)  -> Task a| iTask a
//doLoggedIn t = enterInformation ("Log in","Enter credentials") []
                //>>*  [OnAction ActionOk     (hasValue checkloginandstart)
                     //,OnAction ActionCancel (always (doLoggedIn t))
                      //]
//where checkloginandstart user = checkUser user
							//>>= \ok -> if ok (setLoggedIn user
							//>>| t user) loginfailed
      //checkUser user          = get users
                               //@ \us -> [u\\ u <- us| u.User.name  == user.User.name
                                          //&&  u.User.password  == user.User.password]
                                         //<>
                                         //[]
      //loginfailed			  = showInfo "Login failed... Check username and password."
                                  //>>| doLoggedIn t

//setLoggedIn :: User -> Task ()
//setLoggedIn user
	//= upd (\us -> [{u
				   //&loggedIn = toHidden (fromHidden u.loggedIn
										   //||
										   //user.User.name == u.User.name)}
				   //\\ u <- us]) users @ const ()


lastElems :: Int [a] -> [a]
lastElems n xs = drop (length xs - n) xs

showInfo :: String -> Task String
showInfo msg = viewInformation ("Information","") [] msg


doTasksSequentially :: [Task a] -> Task () | iTask a
doTasksSequentially []     = return ()
doTasksSequentially [t:ts] = t >>| doTasksSequentially ts

allTabs :: [Task a] -> (Task [a]) | iTask a
allTabs ts = allTasks ts  	<<@ ArrangeWithTabs True

allSideBar :: Int UISide Int [Task a] -> (Task [a]) | iTask a
allSideBar b place size ts
	= allTasks ts <<@ (ArrangeWithSideBar b place size True)

c2view :: (Task a) (Task ()) [Task c] [Task d] -> Task () | iTask a  & iTask c & iTask d
c2view main top left right
  = allSideBar 0 TopSide 30
                [ top
                , splitscreenview main left right
                ] @! ()

splitscreenview  main left right
  = allSideBar 1 RightSide 300
                [ innersplitscreenview main left
                , sidebar right
                ] @! ()

innersplitscreenview  main left
  = allSideBar 0 LeftSide 25
              [ sidebar left
              , main @! ()
              ] @! ()

sidebar ts = allSideBar 0 TopSide 25 ts @! ()

chats ::  SDSLens () [ChatMessage] [ChatMessage]
chats = sharedStore "chats" []

derive class iTask ChatMessage

viewChats :: Int -> Task ()
viewChats n = viewSharedInformation "Chats" [] (mapRead (lastElems n) chats) @! ()

chatDialog :: User [Entity] -> Task ()
chatDialog me _ = doOrClose (forever (enterInformation "Type a message" []
                     >>*  [OnAction ActionOk            (hasValue doUpate)])) @! ()
where
 doUpate m =               get currentDateTime
               >>=  \dt -> upd (\cs -> cs ++ [{sender=toString me,when=dt,message=m}]) chats
               @! ()

editChats :: Task ()
editChats  = editSharedList chats

debugstore :: SDSLens () [String] [String]
debugstore = sharedStore "debugstore" []

addDebug :: String -> Task ()
addDebug m = upd (\ms -> ms ++ [m]) debugstore @! ()

showDebug :: Task ()
showDebug = editSharedList debugstore


ppLatLng :: !LatLng -> String
ppLatLng (lat, lng) = ppAngle lat +++ " " +++ ppLat lat +++ " " +++ ppAngle lng +++ " " +++ ppLon lng

ppAngle :: !Angle -> String
ppAngle a
  # dd = toDeg a
  # d  = entier dd
  # dR = toReal d
  # m  = entier ((dd - dR) * 60.0)
  # mR = toReal m
  # s  = (dd - dR - mR / 60.0) * 3600.0
  # s  = roundToNDec 4 s
  = toString d +++ "º " +++ toString m +++ "' " +++ toString s +++ "''"

ppLat :: !Angle -> String
ppLat l
  #! l` = toDeg l
  | l` < 0.0  = toString (roundToNDec 5 (~l`)) +++ "S"
  | otherwise = toString (roundToNDec 5 l`) +++ "N"

ppLon :: !Angle -> String
ppLon l
  #! l` = toDeg l
  | l` < 0.0  = toString (roundToNDec 5 (~l`)) +++ "W"
  | otherwise = toString (roundToNDec 5 l`) +++ "E"

roundToNDec :: !Int !Real -> Real
roundToNDec n r
  #! n` = toReal (10 ^ n)
  = toReal (toInt (r * n`)) / n`

