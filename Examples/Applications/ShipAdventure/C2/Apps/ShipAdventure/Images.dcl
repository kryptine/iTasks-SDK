definition module C2.Apps.ShipAdventure.Images

import C2.Framework.MapEnvironment
import C2.Apps.ShipAdventure.Types
from Graphics.Scalable.Image import :: Image, :: TagSource, :: Image`, :: ImageTag, :: TagRef

:: RenderMode
  = PickRoomMode
  | KitchenMode
  | WalkAroundMode
  | EditMode
  | DOffMode

derive class iTask RenderMode

:: EditHilite = MapHilite !Maps2DIndex | SectionHilite !Maps2DIndex !Coord2D

mapTitleImage :: !Maps2DIndex !(Maybe EditHilite) !Size2D !String -> Image m

maps2DImage :: !(Set Coord3D) !(MapAction SectionStatus) !RenderMode !Maps2D !SectionExitLockMap !SectionHopLockMap !MySectionInventoryMap !MySectionStatusMap !SectionUsersMap !(UserActorMap ObjectType ActorStatus) !(IntMap Device) !Network !*TagSource
            -> Image (Maps2D, MapAction SectionStatus)

map2DImage :: !(Set Coord3D) !(MapAction SectionStatus) !RenderMode !SectionExitLockMap !SectionHopLockMap !MySectionInventoryMap !MySectionStatusMap !SectionUsersMap !(UserActorMap ObjectType ActorStatus) !(IntMap Device) !Network !(!Maps2DIndex, !Map2D)
           -> Image (Maps2D, MapAction SectionStatus)

roomImage :: !Coord3D !SectionExitLockMap !SectionHopLockMap !MyInventory !SectionStatus !MyActors !(IntMap Device) !Network !Bool !Section !Map2D !(MapAction SectionStatus) !*TagSource
          -> Image (Maps2D, MapAction SectionStatus)

editLayoutImage :: !(MapAction SectionStatus) !(IntMap Device) !Network !MySectionInventoryMap !Maps2DIndex !Map2D
                -> Image (Maps2D, MapAction SectionStatus)
