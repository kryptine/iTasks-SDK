definition module C2.Apps.ShipAdventure.Types

import C2.Framework.MapEnvironment
import Data.GenLexOrd
from C2.Apps.ShipAdventure.Images import :: RenderMode

:: MyActor  :== Actor ObjectType ActorStatus
:: MyObject :== Object ObjectType

:: MySectionStatusMap    :== SectionStatusMap SectionStatus
:: MySectionInventoryMap :== SectionInventoryMap ObjectType

:: MyInventory :== IntMap (Object ObjectType)
:: MyActors    :== [Actor ObjectType ActorStatus]

:: MyDrawMapForActor :== DrawMapForActor SectionStatus ObjectType ActorStatus

:: SectionStatus
  = NormalStatus
  | HasSomeWater
  | IsFlooded
  | HasSmoke
  | HasSmallFire
  | HasMediumFire
  | HasBigFire

:: ObjectType   =   FireExtinguisher
                |   FireBlanket
                |   Plug

:: ActorStatus  =   { occupied :: !Availability
                    , health   :: !ActorHealth
                    , energy   :: !ActorEnergy
                    }
:: Availability =   Available  | NotAvailable | Busy
:: ActorHealth  =   FullHealth | AScratch | CompletelyWorn
:: ActorEnergy  =   FullEnergy | SomewhatTired | Fatigued

:: Priority     =   Low | Normal | High | Highest

// logical devices

:: DeviceType =
  { kind            ::  !DeviceKind
  , requires        ::  !Map CableType Capacity
  , produces        ::  !Map CableType Capacity
  }
:: DeviceKind       =   Radar
                    |   Apar
                    |   Sonar
                    |   Hydrophone
                    |   Radio
                    |   PowerGenerator
                    |   CoolingPump
                    |   Gun
                    |   GasTurbine
                    |   DieselEngine
                    |   SmokeDetector
                    |   HeatSensor
                    |   WaterSensor
:: CableType        =   PowerCable | CoolingPipe | DataCable
:: Capacity         :== Int

// physical devices

:: Network =
  { devices         ::  !Map Coord3D [DeviceId]          // [Coord3D |-> DeviceIds]
  , cables          ::  !IntMap Cable                    // [CableId |-> Cable]
  , cableMapping    ::  !IntMap [(Operational, Coord3D)] // [CableId |-> Coord3Ds]
  }
:: Device =
  { description     ::  !String
  , deviceType      ::  !DeviceType
  , deviceId        ::  !DeviceId
  , inCables        ::  ![CableId]
  , outCables       ::  ![CableId]
  }
:: DeviceId         :== Int
:: CableId          :== Int
:: Cable =                                                  // Edge
  { description     :: !String
  , cableId         :: !CableId
  , capacity        :: !Capacity
  , cableType       :: !CableType
  }
:: Operational :== Bool

:: PPDevice =
  { description :: !String
  , deviceType  :: !PPDeviceType
  , deviceId    :: !DeviceId
  , inCables    :: ![CableId]
  , outCables   :: ![CableId]
  }

:: PPDeviceType =
  { kind     :: !DeviceKind
  , requires :: ![(CableType, Capacity)]
  , produces :: ![(CableType, Capacity)]
  }

:: CommandAim =
  { aimDescription       :: !String
  , requiredCapabilities :: ![Capability]
  }

:: Capability
  = Propulsion
  | RadioCommunication
  | SatelliteCommunication
  | AirSensors
  | SurfaceSensors
  | SubsurfaceSensors
  | SurfaceToAirOffence
  | SurfaceToSurfaceOffence
  | SurfaceToSubsurfaceOffence
  | AirDefence
  | SurfaceDefence
  | SubsurfaceDefence

:: CapabilityExpr
  = DeviceExpr DeviceKind
  | CapAndExpr CapabilityExpr CapabilityExpr
  | CapOrExpr  CapabilityExpr CapabilityExpr

:: CapabilityToDeviceKindMap :== Map Capability CapabilityExpr

derive class iTask PPDevice, PPDeviceType, CommandAim, Capability, CapabilityExpr
derive gLexOrd CableType, Capability
derive class iTask ObjectType, ActorStatus, Availability, ActorHealth, ActorEnergy, DeviceType, SectionStatus
derive class iTask Cable, Priority, Network, Device, CableType, DeviceKind

instance == ObjectType
instance == Priority
instance == CableType
instance == Device
instance == DeviceType
instance == DeviceKind
instance == Capability

instance <  CableType
instance <  Capability

instance toString ObjectType
//instance toString Exit
instance toString SectionStatus
instance toString Device

// shared stores:

myUserActorMap   :: UserActorShare ObjectType ActorStatus
myStatusMap      :: SimpleSDSLens MySectionStatusMap
myInventoryMap   :: SimpleSDSLens MySectionInventoryMap
myNetwork        :: SimpleSDSLens Network
myCables         :: SimpleSDSLens (IntMap Cable)
myDevices        :: SimpleSDSLens (IntMap Device)
commandAims      :: SimpleSDSLens [CommandAim]
capabilityMap    :: SimpleSDSLens CapabilityToDeviceKindMap
disabledSections :: SimpleSDSLens (Set Coord3D)

deviceKindsForCapability      :: SDSLens Capability CapabilityExpr    CapabilityExpr
statusInSectionShare          :: SDSLens Coord3D    SectionStatus     SectionStatus
inventoryInSectionShare       :: FocusedSectionInventoryShare ObjectType
deviceIdInNetworkSectionShare :: SDSLens Coord3D    [DeviceId]        [DeviceId]
devicesInSectionShare         :: SDSSequence Coord3D    [Device]          [Device]
deviceWithIdShare             :: SDSLens DeviceId   Device            Device
cableWithIdShare              :: SDSLens CableId    Cable             Cable
cablesInSectionShare          :: SDSLens Coord3D    [Cable]           [Cable]

cablesForSection              :: !Coord3D !Network -> [Cable]

allActiveAlarms    :: SDSLens () [(Coord3D, SectionStatus)] ()
allAvailableActors :: SDSLens () [(Coord3D, MyActor)] ()

// setting and resetting of the detection systems:

setAlarm         :: !User !(!Coord3D, !SectionStatus) !(Shared sds MySectionStatusMap) -> Task () | RWShared sds

// making images from a map

setSectionDetectors :: Task ()

cutCable   :: !Coord3D !CableId !Network -> Network
patchCable :: !Coord3D !CableId !Network -> Network

viewDisabledDevices :: Task ()

hasFire :: !SectionStatus -> Bool

hasSmoke :: !SectionStatus -> Bool

hasWater :: !SectionStatus -> Bool

deviceIsEnabled :: !Device !(IntMap Device) !Network -> Bool

isDetector :: !DeviceKind -> Bool

updateMapStatus :: !RenderMode -> Task (MapAction SectionStatus)

allDisabledDevices :: !(IntMap Device) !Network -> [Device]

allImperiledCommandAims :: !(IntMap Device) !CapabilityToDeviceKindMap ![CommandAim] !Network -> [CommandAim]

deviceIsDisabledInSection :: !Coord3D !Device !(IntMap Device) !Network -> Bool

isOperational :: !CableId !(IntMap [(Operational, Coord3D)]) -> Bool

devicesForCable :: !Cable !(IntMap Device) !Network -> [Device]

devicesForCableInSection :: !Coord3D !Cable !(IntMap Device) !Network -> [Device]

toPPDevice :: !Device -> PPDevice
