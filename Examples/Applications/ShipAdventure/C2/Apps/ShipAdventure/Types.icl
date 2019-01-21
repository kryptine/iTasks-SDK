implementation module C2.Apps.ShipAdventure.Types

//import iTasks

import iTasks.Internal.Tonic
import iTasks.Extensions.Admin.TonicAdmin
import iTasks.Extensions.SVG.SVGEditor
//import Graphics.Scalable
import qualified Data.List as DL
from Data.Func import mapSt
import StdArray
import Data.Data, Data.Either
import qualified Data.IntMap.Strict as DIS
import qualified Data.Map as DM
import qualified Data.Set as DS
import Text.HTML

import C2.Framework.MapEnvironment
from C2.Framework.Logging import addLog
import C2.Apps.ShipAdventure.PathFinding
import C2.Apps.ShipAdventure.Images
import C2.Apps.ShipAdventure.Editor

derive gLexOrd CableType, Capability
derive class iTask ObjectType, ActorStatus, Availability, ActorHealth, ActorEnergy, DeviceType, SectionStatus
derive class iTask Cable, Priority, Network, Device, CableType, DeviceKind, CommandAim, Capability, CapabilityExpr

derive gEditor Set
derive gDefault Set
derive gText Set
derive JSONEncode Set
derive JSONDecode Set

derive JSEncode Map2D, Coord2D, Map, IntMap, Dir, User, Section, Borders, Border, MapAction, Object, Actor
derive JSEncode ObjectType, ActorStatus, Availability, ActorHealth, ActorEnergy, DeviceType, SectionStatus
derive JSEncode Cable, Priority, Network, Device, CableType, DeviceKind, CommandAim, Set, Capability, CapabilityExpr
derive JSDecode Map2D, Coord2D, Map, IntMap, Dir, User, Section, Borders, Border, MapAction, Object, Actor
derive JSDecode ObjectType, ActorStatus, Availability, ActorHealth, ActorEnergy, DeviceType, SectionStatus
derive JSDecode Cable, Priority, Network, Device, CableType, DeviceKind, CommandAim, Set, Capability, CapabilityExpr


// std overloading instances

//instance == Object      where (==) o1 o2 = o1 === o2
instance == ObjectType  where (==) o1 o2 = o1 === o2
instance == Priority    where (==) o1 o2 = o1 === o2
instance toString ObjectType where
  toString FireExtinguisher = "Extinguiser"
  toString FireBlanket      = "Blanket"
  toString Plug             = "Plug"

instance toString Device
where toString {Device | description} = description

instance toString SectionStatus where
  toString NormalStatus  = "NormalStatus"
  toString HasSomeWater  = "HasSomeWater"
  toString IsFlooded     = "IsFlooded"
  toString HasSmoke      = "HasSmoke"
  toString HasSmallFire  = "HasSmallFire"
  toString HasMediumFire = "HasMediumFire"
  toString HasBigFire    = "HasBigFire"


instance < CableType where
  (<) l r = case l =?= r of
              LT -> True
              _  -> False

instance < Capability where
  (<) l r = case l =?= r of
              LT -> True
              _  -> False

instance == CableType where
  (==) l r = l === r

instance == Device where
  (==) l r = l === r

instance == DeviceType where
  (==) l r = l === r

instance == DeviceKind where
  (==) l r = l === r

instance == Capability where
  (==) l r = l === r

// shared stores:

myUserActorMap :: UserActorShare ObjectType ActorStatus
myUserActorMap = sharedStore "myUserActorMap" 'DM'.newMap

myStatusMap :: SDSLens () MySectionStatusMap MySectionStatusMap
myStatusMap = sharedStore "myStatusMap" 'DM'.newMap

statusInSectionShare :: SDSLens Coord3D SectionStatus SectionStatus
statusInSectionShare = mapLens "statusInSectionShare" myStatusMap (Just NormalStatus)

deviceKindsForCapability :: SDSLens Capability CapabilityExpr CapabilityExpr
deviceKindsForCapability
  = mapLens "deviceKindsForCapability" capabilityMap Nothing

myInventoryMap :: SDSLens () MySectionInventoryMap MySectionInventoryMap
myInventoryMap = sharedStore "myInventoryMap" 'DM'.newMap

viewDisabledDevices :: Task ()
viewDisabledDevices = viewSharedInformation "Disabled devices" [ViewAs (\(nw, ds) -> map toPPDevice (allDisabledDevices ds nw))] (myNetwork |*| myDevices) @! ()

//manageDevices :: Bool -> Task ()
//manageDevices kitchen
  //| kitchen
  //=               get myNetwork
  //>>= \network -> viewDisabledDevices
  //>>*             [  OnAction (Action (if enabled "/Cut " "/Patch " +++ mkCableDesc roomNo cable) []) (always (if enabled (cutCableTask roomNo cable) (patchCableTask roomNo cable)))
                  //\\ cable  <- 'DIS'.elems network.cables
                  //,  (enabled, roomNo) <- fromMaybe [] ('DIS'.get cable.cableId network.cableMapping)
                  //]
  //| otherwise = viewDisabledDevices
  //where
  //ppNetwork :: !Network -> ((String, [(String, [PPDevice])]), (String, [Cable]))
  //ppNetwork network = (devicesView, cablesView)
    //where
    //cablesView :: (String, [Cable])
    //cablesView = ("Cables", 'DIS'.elems network.cables)

    //devicesView :: (String, [(String, [PPDevice])])
    //devicesView = ( "Devices"
                  //, [  ("Section " +++ toString roomNo, [toPPDevice d \\ d <- devices` | not (isDetector d.Device.deviceType.DeviceType.kind)])
                    //\\ (roomNo, devices`) <- 'DIS'.toList network.devices
                    //]
                  //)
  //mkCableDesc :: !Coord3D !Cable -> String
  //mkCableDesc roomNo {Cable | cableId, description}
    //= "'" +++ description +++ " " +++ toString cableId +++ "', room " +++ toString roomNo
  //cutCableTask :: !Coord3D !Cable -> Task ()
  //cutCableTask roomNo cable
    //= upd (cutCable roomNo cable.cableId) myNetwork >>| manageDevices kitchen
  //patchCableTask :: !Coord3D !Cable -> Task ()
  //patchCableTask roomNo cable
    //= upd (patchCable roomNo cable.cableId) myNetwork >>| manageDevices kitchen

allImperiledCommandAims :: !(IntMap Device) !CapabilityToDeviceKindMap ![CommandAim] !Network -> [CommandAim]
allImperiledCommandAims deviceMap caps cas network
  # allDisabledDevs     = allDisabledDevices deviceMap network
  # allDisabledDevKinds = map (\d -> d.Device.deviceType.DeviceType.kind) allDisabledDevs
  # disabledCaps        = allDisabledCapabilities allDisabledDevKinds caps
  = [ ca \\ ca <- cas
    | isImperiled disabledCaps ca]

isImperiled :: ![Capability] !CommandAim -> Bool
isImperiled disabledCaps {requiredCapabilities} = not ('DS'.null ('DS'.intersection ('DS'.fromList requiredCapabilities) ('DS'.fromList disabledCaps)))

allDisabledCapabilities :: ![DeviceKind] !CapabilityToDeviceKindMap -> [Capability]
allDisabledCapabilities disDevKinds caps
  = [ cap \\ (cap, devAlg) <- 'DM'.toList caps
    | canProvideCapability devAlg disDevKinds]

canProvideCapability :: !CapabilityExpr ![DeviceKind] -> Bool
canProvideCapability (DeviceExpr dk)  dks = 'DL'.isMember dk dks
canProvideCapability (CapAndExpr l r) dks = canProvideCapability l dks && canProvideCapability r dks
canProvideCapability (CapOrExpr  l r) dks = canProvideCapability l dks || canProvideCapability r dks

deviceIsEnabled :: !Device !(IntMap Device) !Network -> Bool
deviceIsEnabled dev deviceMap network
  = isEmpty [ d \\ d <- allDisabledDevices deviceMap network
            | d.Device.deviceId == dev.Device.deviceId]

allDisabledDevices :: !(IntMap Device) !Network -> [Device]
allDisabledDevices deviceMap network
  = 'DL'.nub (flatten [  devicesForCable cable deviceMap network
                      \\ cable <- 'DIS'.elems network.cables
                      |  not (isOperational cable.cableId network.cableMapping)
                      ])

// TODO Make more efficient
deviceIsDisabledInSection :: !Coord3D !Device !(IntMap Device) !Network -> Bool
deviceIsDisabledInSection roomNo device deviceMap network
  #! disabledDevs = disabledDevicesForSection roomNo deviceMap network
  = not (isEmpty [0 \\ dev <- disabledDevs | dev.Device.deviceId == device.Device.deviceId])

disabledDevicesForSection :: !Coord3D !(IntMap Device) !Network -> [Device]
disabledDevicesForSection roomNo deviceMap network
  = 'DL'.nub (flatten [  devicesForCableInSection roomNo cable deviceMap network
                      \\ cable <- 'DIS'.elems network.cables
                      |  not (isOperational cable.cableId network.cableMapping)
                      ])

devicesForCable :: !Cable !(IntMap Device) !Network -> [Device]
devicesForCable cable=:{cableId} deviceMap {cableMapping, devices}
  = 'DL'.nub [  device
             \\ (_, roomNo) <- fromMaybe [] ('DIS'.get cableId cableMapping)
             ,  deviceId    <- fromMaybe [] ('DM'.get roomNo devices)
             ,  Just device <- ['DIS'.get deviceId deviceMap]
             |  requiresCable cable device
             ]
  where
  // TODO Take resource quantity and redundancy into account
  requiresCable :: !Cable !Device -> Bool
  requiresCable { Cable | cableType} device
    = isJust ('DM'.get cableType device.Device.deviceType.DeviceType.requires)

devicesForCableInSection :: !Coord3D !Cable !(IntMap Device) !Network -> [Device]
devicesForCableInSection roomNo cable=:{cableId} deviceMap {cableMapping, devices}
  = 'DL'.nub [  device
             \\ (_, roomNo`) <- fromMaybe [] ('DIS'.get cableId cableMapping)
             ,  deviceId     <- fromMaybe [] ('DM'.get roomNo` devices)
             ,  Just device <- ['DIS'.get deviceId deviceMap]
             |  roomNo == roomNo` && requiresCable cable device
             ]
  where
  // TODO Take resource quantity and redundancy into account
  requiresCable :: !Cable !Device -> Bool
  requiresCable { Cable | cableType} device = isJust ('DM'.get cableType device.Device.deviceType.DeviceType.requires)

derive class iTask PPDevice, PPDeviceType

toPPDevice :: !Device -> PPDevice
toPPDevice { Device | description, deviceType, deviceId, inCables, outCables } = { PPDevice
                                                                                 | description = description
                                                                                 , deviceType  = toPPDeviceType deviceType
                                                                                 , deviceId    = deviceId
                                                                                 , inCables    = inCables
                                                                                 , outCables   = outCables
                                                                                 }

toPPDeviceType :: !DeviceType -> PPDeviceType
toPPDeviceType { DeviceType | kind, requires, produces } = { PPDeviceType
                                                           | kind     = kind
                                                           , requires = 'DM'.toList requires
                                                           , produces = 'DM'.toList produces
                                                           }

isOperational :: !CableId !(IntMap [(!Operational, !Coord3D)]) -> Bool
isOperational cableId cableMapping = and [b \\ (b, _) <- fromMaybe [] ('DIS'.get cableId cableMapping)]

smokeDetector :: DeviceType
smokeDetector  = { DeviceType
                 | kind     = SmokeDetector
                 , requires = 'DM'.fromList []
                 , produces = 'DM'.fromList []
                 }

heatSensor :: DeviceType
heatSensor     = { DeviceType
                 | kind     = HeatSensor
                 , requires = 'DM'.fromList []
                 , produces = 'DM'.fromList []
                 }

waterSensor :: DeviceType
waterSensor    = { DeviceType
                 | kind     = WaterSensor
                 , requires = 'DM'.fromList []
                 , produces = 'DM'.fromList []
                 }

mkAllSensors :: !DeviceId !DeviceId !DeviceId -> [Device]
mkAllSensors sd hs ws
  = [ { Device
      | description = "Smoke detector " +++ toString sd
      , deviceType  = smokeDetector
      , deviceId    = sd
      , inCables    = []
      , outCables   = []
      }
    , { Device
      | description = "Heat sensor " +++ toString hs
      , deviceType  = heatSensor
      , deviceId    = hs
      , inCables    = []
      , outCables   = []
      }
    , { Device
      | description = "Water sensor " +++ toString ws
      , deviceType  = waterSensor
      , deviceId    = ws
      , inCables    = []
      , outCables   = []
      }
    ]

// my physical mapping of the devices in a network

deviceWithIdShare :: SDSLens DeviceId Device Device
deviceWithIdShare = intMapLens "deviceWithIdShare" myDevices Nothing

deviceIdInNetworkSectionShare :: SDSLens Coord3D [DeviceId] [DeviceId]
deviceIdInNetworkSectionShare = sdsLens "deviceIdInNetworkSectionShare" (const ()) (SDSRead read) (SDSWrite write) (SDSNotify notify) Nothing myNetwork
  where
  read :: !Coord3D !Network -> MaybeError TaskException [DeviceId]
  read c3d network = Ok (fromMaybe [] ('DM'.get c3d network.devices))

  write :: !Coord3D !Network ![DeviceId] -> MaybeError TaskException (Maybe Network)
  write c3d network devIds = Ok (Just ({network & devices = 'DM'.put c3d devIds network.devices}))

  notify :: !Coord3D !Network ![DeviceId] -> SDSNotifyPred Coord3D
  notify c3d network devIds = \_ idx` -> c3d == idx`

devicesInSectionShare :: SDSSequence Coord3D [Device] [Device]
devicesInSectionShare
  = sdsSequence "devicesInSectionShare" id mkP2 (\_ _ -> Right mkR) (SDSWrite write1) (SDSWrite write2) deviceIdInNetworkSectionShare myDevices
  where
  mkP2 :: Coord3D [DeviceId] -> ()
  mkP2 _ devIds = () // fromMaybe [] ('DM'.get c3d network.devices)
  mkR :: !(![DeviceId], !IntMap Device) -> [Device]
  mkR (devIds, allDevs) = [dev \\ devId <- devIds, Just dev <- ['DIS'.get devId allDevs]]
  write1 :: Coord3D [DeviceId] ![Device] -> MaybeError TaskException (Maybe [DeviceId])
  write1 _ _ devices = Ok (Just [dev.Device.deviceId \\ dev <- devices])
  write2 :: Coord3D !(IntMap Device) ![Device] -> MaybeError TaskException (Maybe (IntMap Device))
  write2 _ deviceMap devices = Ok (Just (foldr (\device deviceMap -> 'DIS'.put device.Device.deviceId device deviceMap) deviceMap devices))

myDevices :: SDSLens () (IntMap Device) (IntMap Device)
myDevices = sharedStore "myDevices" devices
  where
  devices = 'DIS'.fromList [  f dt
                           \\ i <- [1,4..420]
                            , dt <- mkAllSensors i (i + 1) (i + 2)]
  f :: !Device -> (!DeviceId, !Device)
  f dev = (dev.Device.deviceId, dev)

commandAims :: SDSLens () [CommandAim] [CommandAim]
commandAims = sharedStore "commandAims" []

capabilityMap :: SDSLens () CapabilityToDeviceKindMap CapabilityToDeviceKindMap
capabilityMap = sharedStore "capabilityMap" ('DM'.fromList defaultList)
  where
  defaultList
    = [ (Propulsion,         (cap GasTurbine + cap DieselEngine))
      , (AirSensors,         (cap Apar + cap Radar))
      , (SurfaceSensors,     (cap Apar + cap Radar))
      , (SubsurfaceSensors,  (cap Sonar + cap Hydrophone))
      , (RadioCommunication, cap Radio)
      ]

instance + CapabilityExpr where
  (+) l r = CapOrExpr l r

instance * CapabilityExpr where
  (*) l r = CapAndExpr l r

cap :: DeviceKind -> CapabilityExpr
cap k = DeviceExpr k

myNetwork :: SDSLens () Network Network
myNetwork = sharedStore "myNetwork"
  { Network
  | devices      = 'DM'.newMap
  , cables       = 'DIS'.newMap
  , cableMapping = 'DIS'.newMap
  }

myCables :: SDSLens () (IntMap Cable) (IntMap Cable)
myCables = sdsProject (SDSLensRead read) (SDSLensWrite write) Nothing myNetwork
  where
  read :: !Network -> MaybeError TaskException (IntMap Cable)
  read { Network | cables } = Ok cables
  write :: !Network !(IntMap Cable) -> MaybeError TaskException (Maybe Network)
  write network cables      = Ok (Just {network & cables = cables})

cablesInSectionShare :: SDSLens Coord3D [Cable] [Cable]
cablesInSectionShare = sdsLens "cablesInSectionShare" (const ()) (SDSRead read) (SDSWrite write) (SDSNotify notify) Nothing myNetwork
  where
  read :: !Coord3D !Network -> MaybeError TaskException [Cable]
  read c3d network = Ok (cablesForSection c3d network)
  write :: !Coord3D !Network ![Cable] -> MaybeError TaskException (Maybe Network)
  write c3d network cables = Ok (Just (foldr (\cable network -> let coords = fromMaybe [] ('DIS'.get cable.cableId network.cableMapping)
                                                                    inList = not (isEmpty [0 \\ (_, c3d`) <- coords | c3d === c3d`])
                                                                in  if inList network
                                                                      {network & cableMapping = 'DIS'.put cable.cableId [(True, c3d) : coords] network.cableMapping}
                                                                ) network cables))
  notify :: !Coord3D !Network ![Cable] -> SDSNotifyPred Coord3D
  notify c3d oldNetwork newCables = \_ c3d` -> c3d === c3d`

cablesForSection :: !Coord3D !Network -> [Cable]
cablesForSection c3d { Network | cables, cableMapping }
  = case [cid \\ (cid, ms) <- 'DIS'.toList cableMapping, (_, c3d`) <- ms | c3d == c3d`] of
      [] -> []
      xs -> [cable \\ Just cable <- (map (\cid -> 'DIS'.get cid cables) xs)]

cableWithIdShare :: SDSLens CableId Cable Cable
cableWithIdShare = intMapLens "cableWithIdShare" myCables Nothing

cutCable :: !Coord3D !CableId !Network -> Network
cutCable roomNo cableId network = { network & cableMapping = 'DIS'.alter (fmap (\xs -> [(if (no == roomNo) False op, no) \\ (op, no) <- xs])) cableId network.cableMapping }

patchCable :: !Coord3D !CableId !Network -> Network
patchCable roomNo cableId network = { network & cableMapping = 'DIS'.alter (fmap (\xs -> [(if (no == roomNo) True op, no) \\ (op, no) <- xs])) cableId network.cableMapping }

inventoryInSectionShare :: FocusedSectionInventoryShare ObjectType
inventoryInSectionShare = mapLens "inventoryInSectionShare" myInventoryMap (Just 'DIS'.newMap)

allAvailableActors :: SDSLens () [(!Coord3D, !MyActor)] ()
allAvailableActors
  = /*toReadOnly */ (sdsProject (SDSLensRead readActors) (SDSBlindWrite \_. Ok Nothing) Nothing (sectionUsersShare |*| myUserActorMap))
  where
  readActors :: !(SectionUsersMap, UserActorMap ObjectType ActorStatus) -> MaybeError TaskException [(!Coord3D, !MyActor)]
  readActors (sectionUsersMap, userActorMap)
    = Ok [(c3d, a) \\ us <- 'DM'.elems sectionUsersMap
                    , u  <- us
                    , Just (c3d, a) <- [findUser u sectionUsersMap userActorMap]
                    | a.actorStatus.occupied === Available]

allActiveAlarms :: SDSLens () [(!Coord3D, !SectionStatus)] ()
allActiveAlarms
  = /*toReadOnly */ (sdsProject (SDSLensRead readAlarms) (SDSBlindWrite \_. Ok Nothing) Nothing myStatusMap)
  where
  readAlarms :: !MySectionStatusMap -> MaybeError TaskException [(!Coord3D, !SectionStatus)]
  readAlarms statusMap = Ok [ (number, status) \\ (number, status) <- 'DM'.toList statusMap
                              | isHigh status]

// detectors setting

// setting and resetting of the detection systems
setSectionDetectors :: Task ()
setSectionDetectors
  = updateMapStatus KitchenMode
    >>* [OnValue (\tv -> case tv of
                           Value (SetStatus c3d st)   _ -> Just (setSectionStatus c3d st myStatusMap >>| setSectionDetectors)
                           Value (ToggleDoor c3d d)   _ -> Just (toggleDoor c3d d >>| setSectionDetectors)
                           Value (ToggleHop c3d c3d`) _ -> Just (toggleHop c3d c3d` >>| setSectionDetectors)
                           _                            -> Nothing
                 )]

updateMapStatus :: !RenderMode -> Task (MapAction SectionStatus)
updateMapStatus mode
  = /* project (\tv _ -> case tv of
                          Value x _ -> Just x
                          _         -> Nothing) sharedMapAction */
      (updateInformationWithShared "Map Status"
        [UpdateUsing id (const snd) editor]
        (disabledSections |*| maps2DShare |*| lockedExitsShare |*| lockedHopsShare |*| myInventoryMap |*| myStatusMap |*| sectionUsersShare |*| myUserActorMap |*| myNetwork |*| myDevices)
        NoAction)
where
	editor = fromSVGEditor
		{ initView = \((((((((((_, ms2d), _), _), _), _), _), _), _), _), cl) -> (ms2d, cl)
		, renderImage = \((((((((((disSects, _), exitLocks), hopLocks), inventoryMap), statusMap), sectionUsersMap), userActorMap), network), allDevices), cl) (ms2d`, cl`)
			-> maps2DImage disSects cl mode ms2d` exitLocks hopLocks inventoryMap statusMap sectionUsersMap userActorMap allDevices network
		, updModel = \((((((((((disSects, _), exitLocks), hopLocks), inventoryMap), statusMap), sectionUsersMap), userActorMap), network), allDevices), _)  (ms2d`, cl`) -> ((((((((((disSects, ms2d`), exitLocks), hopLocks), inventoryMap), statusMap), sectionUsersMap), userActorMap), network), allDevices), cl`)
		}

disabledSections :: SDSLens () (Set Coord3D) (Set Coord3D)
disabledSections = sharedStore "disabledSections" 'DS'.newSet

updateSectionStatus :: !Coord3D -> Task (MapAction SectionStatus)
updateSectionStatus c3d=:(floorIdx, _)
  = updateInformationWithShared "Section Status"
      [UpdateUsing id (const snd) editor]
      (maps2DShare |*| lockedExitsShare |*| lockedHopsShare |*| sdsFocus c3d inventoryInSectionShare |*| sdsFocus c3d statusInSectionShare |*| sdsFocus c3d (actorsInSectionShare myUserActorMap) |*| myNetwork |*| myDevices)
      NoAction
where
	editor = fromSVGEditor
		{ initView    = \((((((((ms2d, _), _), _), _), _), _), _), cl) -> (ms2d, cl)
		, renderImage = \((((((((ms2d, exitLocks), hopLocks), inventoryMap), statusMap), actorMap), network), allDevices), cl) (ms2d`, cl`)
			-> roomImage c3d exitLocks hopLocks inventoryMap statusMap actorMap allDevices network True (fromJust (getSectionFromMap c3d ms2d`)) (ms2d !! floorIdx) cl`
		, updModel    = \((((((((_,    exitLocks), hopLocks), inventoryMap), statusMap), actorMap), network), allDevices), _)  (ms2d`, cl`)
			-> ((((((((ms2d`, exitLocks), hopLocks), inventoryMap), statusMap), actorMap), network), allDevices), cl`)
		}

setAlarm :: !User !(!Coord3D, !SectionStatus) !(Shared sds MySectionStatusMap) -> Task () | RWShared sds
setAlarm user (alarmLoc, status) shStatusMap
  =   setSectionStatus alarmLoc status shStatusMap
  >>| addLog user ""  ("Resets " <+++ status <+++ " in Section " <+++ alarmLoc <+++ " to False.")

setSectionStatus :: !Coord3D !SectionStatus !(Shared sds (SectionStatusMap SectionStatus)) -> Task () | RWShared sds
setSectionStatus roomNumber status statusMap
  = upd ('DM'.put roomNumber status) statusMap @! ()

hasFire :: !SectionStatus -> Bool
hasFire HasSmallFire  = True
hasFire HasMediumFire = True
hasFire HasBigFire    = True
hasFire _             = False

hasSmoke :: !SectionStatus -> Bool
hasSmoke HasSmoke = True
hasSmoke _        = False

hasWater :: !SectionStatus -> Bool
hasWater HasSomeWater = True
hasWater IsFlooded    = True
hasWater _            = False


isHigh :: !SectionStatus -> Bool
isHigh status = hasFire status || hasSmoke status || hasWater status

isDetector :: !DeviceKind -> Bool
isDetector SmokeDetector = True
isDetector HeatSensor    = True
isDetector WaterSensor   = True
isDetector _             = False

