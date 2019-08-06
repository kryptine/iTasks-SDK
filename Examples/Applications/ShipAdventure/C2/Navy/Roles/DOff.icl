implementation module C2.Navy.Roles.DOff

import iTasks
import C2.Framework.Entity
import C2.Apps.ShipAdventure.Scripting, C2.Apps.ShipAdventure.Core, C2.Apps.ShipAdventure.Types, C2.Apps.ShipAdventure.Images, C2.Framework.Logging
from Data.Set import :: Set
import qualified Data.Set as DS
from Data.IntMap.Strict import instance Functor IntMap
import qualified Data.IntMap.Strict as DIS
import C2.Apps.ShipAdventure.Editor
import C2.Apps.ShipAdventure.Types
import Data.Map.GenJSON
import Data.Functor

dOffRegisterEntity  :: [User -> Task Entity]
dOffRegisterEntity = []

dOffContinuousTasks :: [User [Entity] -> Task ()]
dOffContinuousTasks = []

dOffAlwaysOnTasks :: [(String, User [Entity] -> Task ())]
dOffAlwaysOnTasks = [ ("Damage control",    \_ _ -> damageControl)
                    , ("Damage prediction", \_ _ -> damagePrediction)
                    ]

dOffMap :: Task ()
dOffMap
  # me = AuthenticatedUser "doff" ["doff"] (Just "D-Officer")
  =   set me currentUser
  >>| updateMapStatus DOffMode @! ()

damageControl :: Task ()
damageControl
  = allTasks [ viewDisabledDevices @! ()
             , giveInstructions @! ()
             ] <<@ ArrangeHorizontal @! ()

damagePrediction :: Task ()
damagePrediction
  =    watch disabledSections
  -&&- (((((viewSharedInformation "Selected section" [] sharedMapAction <<@ ArrangeVertical)
       -|| showDisabledDevices <<@ ArrangeVertical) <<@ ArrangeVertical)
       -|| ((showImperiledCommandAims <<@ ArrangeHorizontal)
       -|| showCommandAims <<@ ArrangeVertical)) <<@ ArrangeHorizontal)
  >>* [ OnAction (Action "/Disable" ) disableSection
      , OnAction (Action "/Enable" )  enableSection
      , OnAction (Action "/Reset" )   (always resetSections)
      ]
  where
  disableSection (Value (disSects, FocusOnSection c3d) _)
    | not (isDisabled c3d disSects) = Just (upd ('DS'.insert c3d) disabledSections >>| damagePrediction)
  disableSection _ = Nothing
  enableSection (Value (disSects, FocusOnSection c3d) _)
    | isDisabled c3d disSects = Just (upd ('DS'.delete c3d) disabledSections >>| damagePrediction)
  enableSection _ = Nothing
  resetSections = set 'DS'.newSet disabledSections >>| damagePrediction
  isDisabled c3d disSects = 'DS'.member c3d disSects

derive gEditor Set
derive gDefault Set
derive gText Set
derive JSONEncode Set
derive JSONDecode Set

showCommandAims :: Task ()
showCommandAims = viewSharedInformation "Current Command Aims" [] commandAims @! ()

showImperiledCommandAims :: Task ()
showImperiledCommandAims
  = viewSharedInformation "Imperiled Command Aims"
      [ViewAs (\((((disSects, nw), devs), caps), cas) -> allImperiledCommandAims devs caps cas (doDisableCablesInSections disSects nw))]
      (disabledSections |*| myNetwork |*| myDevices |*| capabilityMap |*| commandAims) @! ()

showDisabledDevices :: Task ()
showDisabledDevices
  = viewSharedInformation "Disabled devices"
      [ViewAs (\((disSects, nw), devs) -> map (\d -> d.Device.description) (allDisabledDevices devs (doDisableCablesInSections disSects nw)))]
      (disabledSections |*| myNetwork |*| myDevices) @! ()

doDisableCablesInSections :: !(Set Coord3D) !Network -> Network
doDisableCablesInSections disabledSections nw = {nw & cableMapping = fmap doDisable nw.cableMapping}
  where
  doDisable xs = [(not ('DS'.member c3d` disabledSections), c3d`) \\ (operational, c3d`) <- xs]

dOffOptionalTasks :: [(String, User [Entity] -> Task ())]
dOffOptionalTasks = [("Show log", \_ _ -> showLog @! ())]
