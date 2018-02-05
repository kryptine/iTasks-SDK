implementation module C2.Navy.Roles.Commander

import iTasks
import iTasks.Extensions.Admin.TonicAdmin, iTasks.Internal.Tonic
import Text, C2.Framework.Core, C2.Framework.Util, C2.Framework.Entity
import iTasks.Extensions.Document
import iTasks.Extensions.GIS.GoogleMap
from Data.IntMap.Strict import :: IntMap, instance Functor IntMap
import qualified Data.IntMap.Strict as DIS
import C2.Framework.ContactPosition
import C2.Framework.Common
import Math.Geometry
import Data.Maybe
import StdMisc
import qualified Data.List as DL
import C2.Apps.ShipAdventure.Core, C2.Apps.ShipAdventure.Types

:: Weapon
  = SuperRapid76mm
  | MarlinWS30mm
  | HitroleNT12d7mm
  | FNMAG7d62mm

derive class iTask Weapon

:: Precision
  = ShotAcrossTheBow
  | DirectHit

derive class iTask Precision

:: IntelReport
  = { sender  :: String
    , date    :: DateTime
    , kind    :: ReportType
    , inhoud  :: String
    , bijlage :: Maybe Document
    }

:: ReportType = Niets | Terrorist TerInfo | Weapon WeaponInfo

:: TerInfo    = {name :: String, address :: Maybe String}
:: WeaponInfo = {name :: String, range :: Int}

derive class iTask IntelReport, ReportType, TerInfo, WeaponInfo

commanderRegisterEntity :: [User -> Task Entity]
commanderRegisterEntity = [\_ -> registerEntity (\idx -> newMovingEntityWithSpeedAndDirection idx (deg 1.68, deg 45.89) 500.0 (deg 45.0))
                          ]

commanderContinuousTasks :: [User [Entity] -> Task ()]
commanderContinuousTasks = [ \_ ses -> allTasks (map (\{Entity|e_id} -> periodicallyUpdateEntity e_id) ses) @! ()
                           ]

commanderAlwaysOnTasks :: [(String, User [Entity] -> Task ())]
//commanderAlwaysOnTasks = [ ("Map", \cu es -> mapView myNetwork (deviceIsEnabled myOnlyRadar) cu es) // TODO FIXME
commanderAlwaysOnTasks = [ ("Map", \cu es -> mapView myNetwork (const True) cu es)
                         , ("Command aim", \_ _ -> defineCommandAims)
                         ]

defineCommandAims :: Task ()
defineCommandAims = updateSharedInformation (Title "Command aims") [] commandAims @! ()

commanderOptionalTasks :: [(String, User [Entity] -> Task ())]
commanderOptionalTasks =
  [ ("Report taak uitzetten",    taakReportUitzetten)
  , ("Manage Messages",          chatManagement)
  , ("Broadcast message",        chatDialog)
  , ("Notification",             doNote)
  , ("Entity Speed Watch",       entityWatch)
  , ("Use gun",                  useGun)
  , ("Set intercept course",     setInterceptCourse)
  , ("Communicate with contact", communicateWithContact)
  , ("Maak rapport",             makeReport)
  ]

taakReportUitzetten sender _
  =            enterChoiceWithShared "Kies een gebruiker" [] users
  >>= \user -> enterInformation "Korte beschrijving" []
  >>= \des ->  enterInformation "Kies prioriteit" []
  >>= \prio -> addTaskForUserAndReport des user sender prio stelOpReport @! ()

makeReport:: User [Entity] -> Task ()
makeReport user _ = stelOpReport user >>= verstuur

stelOpReport :: User -> Task IntelReport
stelOpReport user
  =          get currentDateTime
  >>= \dt -> updateInformation "Geef details" []
               { IntelReport
               | sender  = toString user
               , date    = dt
               , kind    = Niets
               , inhoud  = ""
               , bijlage = Nothing
               }

verstuur :: IntelReport -> Task ()
verstuur report
  =            enterChoiceWithShared "Verzenden naar" [] users
  >>= \user -> enterInformation "Onderwerp" []
  >>= \des ->  enterInformation "Kies prioriteit" []
  >>= \prio -> addCancebleTaskForUser des user prio (\user -> viewInformation "Report" [] report @! ()) @! ()


communicateWithContact :: User [Entity] -> Task ()
communicateWithContact sender _ = selectedContact @! () // TODO Implement

setInterceptCourse :: User [Entity] -> Task ()
setInterceptCourse sender [ownEntity]
  =          viewInformation "Setting intercept course towards contact" [ViewAs mkPPEntity] ownEntity @! ()
setInterceptCourse sender ownEntities
  =          enterMultipleChoice "Select your ships" [] ownEntities
  >>= \es -> viewSharedInformation "Setting intercept course towards contact with own ships" [ViewAs (map mkPPEntity o appendSelection es)] selectedContactShare @! ()
  where
  appendSelection es (Just x) = [x : es]
  appendSelection es _        = es

selectedContact :: Task (Maybe Entity)
selectedContact = viewSharedInformation "Selected target" entityView selectedContactShare
  where
  entityView = [ViewAs (maybe {PPEntity | id = 0, position = "No entity selected"} mkPPEntity)]

useGun :: User [Entity] -> Task ()
useGun sender _
  =   selectedContact -&&- chooseWeapon -&&- choosePrecision
  >>* [OnAction ActionContinue (ifValue hasContact continue)]
  where
  hasContact :: (Maybe Entity, (Weapon, Precision)) -> Bool
  hasContact (Just _, (_ , _)) = True
  hasContact _                 = False

  continue (contact, (weapon, precision))
    =   ("WSO", "Load gun") @: (loadGunTask sender weapon)
    >>| viewInformation "Please confirm" [] "Fire?"
    >>* [ OnAction ActionYes (always (viewInformation "Notification" [] "BOOOOM!!!" @! ()))
        , OnAction ActionNo  (always (viewInformation "Notification" [] "Firing sequence aborted" @! ()))]

chooseWeapon :: Task Weapon
chooseWeapon = enterInformation "Choose weapon" []

choosePrecision :: Task Precision
choosePrecision = enterInformation "Choose precision" []

loadGunTask :: User Weapon -> Task ()
loadGunTask sender weapon
  =                            (enterChoiceWithShared "Kies een gebruiker" [] users
                               -&&- enterInformation "Kies prioriteit" [])
  >>= \(selectedUser, prio) -> if (isOnBoard selectedUser)
                                 (addTaskForUserAndReport "Load gun" selectedUser sender prio stelOpReport @! ())
                                 (viewInformation "Problem" [] "User is not on board to perform task" @! ())

isOnBoard :: User -> Bool
//isOnBoard {location = Hidden (Just (OnBoard _))} = True
isOnBoard _                                      = False

entityWatch :: User [Entity] -> Task ()
entityWatch sender _
  =         enterInformation "Geef snelheid voor waarschuwing" []
  >>= \v -> makeWatchTask "Fast entity" sender Urgent selectedContactShare (maybe False (movesFasterThan v)) (showEntity selectedContactShare)
  where
  movesFasterThan v {e_position = MovingPos { mp_speed }} = mp_speed >= v
  movesFasterThan _ _                                     = False

showEntity e v = viewSharedInformation "Fast flying entity" [] e >>| return ()

entityLiveView :: String Entity -> Task ()
entityLiveView d {Entity | e_id} = viewSharedInformation d [ViewAs (maybe (abort ":(") mkPPEntity)] (sdsFocus e_id contactWithId) @! ()

doNote us _ = doOrClose (forever (enterInformation "Make a note" [] >>= \nf -> addNotification nf)) @! ()

chatManagement user _ = editChats

mkPPEntity :: Entity -> PPEntity
mkPPEntity se
  = { PPEntity
    | id             = se.Entity.e_id
    , position       = ppPos se.Entity.e_position
    }

:: PPEntity =
  { id             :: !Int
  , position       :: !String
  }

derive class iTask PPEntity

ppPos (RelPos rp) = "x: " +++ toString rp.rp_x +++ ", y: " +++ toString rp.rp_y +++ ", z: " +++ toString rp.rp_z
ppPos (MovingPos mp) = ppLatLng mp.mp_position +++ " " +++ toString mp.mp_altitude +++ " " +++ ppAngle mp.mp_direction +++ " " +++ toString mp.mp_speed +++ " " +++ toString mp.mp_vertical_speed
ppPos (GeoPos ll) = ppLatLng ll

