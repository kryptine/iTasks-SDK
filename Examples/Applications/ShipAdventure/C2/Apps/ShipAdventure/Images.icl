implementation module C2.Apps.ShipAdventure.Images

import C2.Framework.MapEnvironment
import C2.Apps.ShipAdventure.Core
import C2.Apps.ShipAdventure.Types

import qualified Graphics.Scalable.Image as GS
import Graphics.Scalable.Image => qualified grid
import Graphics.Scalable.Types
//import qualified Graphics.Scalable as GS
//
//from Graphics.Scalable import <@<, class tuneImage
//from Graphics.Scalable import px, rect, normalFontDef, overlay, above, text, scale, beside, empty, collage, line, xline, yline, polygon
//from Graphics.Scalable import class toSVGColor(..), class margin(..), class *.(..), instance toSVGColor String
//from Graphics.Scalable import instance tuneImage OnClickAttr, instance tuneImage FillAttr, instance tuneImage OpacityAttr
//from Graphics.Scalable import instance tuneImage StrokeAttr, instance tuneImage DashAttr, instance tuneImage MaskAttr, instance tuneImage StrokeWidthAttr
//from Graphics.Scalable import instance margin (Span,Span), instance margin (Span,Span,Span), instance *. Span, instance zero Span
//from Graphics.Scalable import :: Span, :: FontDef, :: DashAttr(..), :: StrokeAttr(..), :: FillAttr(..), :: OnClickAttr(..), :: OpacityAttr(..), :: MaskAttr(..)
//from Graphics.Scalable import :: StrokeWidthAttr(..), :: Host(..), :: ImageOffset, :: XYAlign(..), :: XAlign(..), :: YAlign(..), :: Slash(..), :: Markers

import qualified Data.IntMap.Strict as DIS
import qualified Data.Map as DM
import qualified Data.Set as DS
import qualified Data.List as DL
import Data.Maybe

derive class iTask RenderMode

mapFont p                     = normalFontDef "Verdana" p
mapTitleFontSize             =: 10.0
editSectionBackgroundColor   =: toSVGColor "lightsteelblue"
hiliteSectionBackgroundColor =: toSVGColor "lightskyblue"
editSectionDashColor         =: toSVGColor "white"


mapTitleImage :: !Maps2DIndex !(Maybe EditHilite) !Size2D !String -> Image m
mapTitleImage idx hilite size2D=:(w, _) mapId
  = margin (px zero, px zero, px (0.5 * mapTitleFontSize))
      (overlay [(AtMiddleX, AtMiddleY)] []
               [text (mapFont mapTitleFontSize) mapId]
               (Host (rect (px w) (px (2.0 * mapTitleFontSize)) <@< {fill = if (hiliteThisMap hilite idx) hiliteSectionBackgroundColor (toSVGColor "white")})))

// making an image from the map ...
maps2DImage :: !(Set Coord3D) !(MapAction SectionStatus) !RenderMode !Maps2D !SectionExitLockMap !SectionHopLockMap !MySectionInventoryMap !MySectionStatusMap !SectionUsersMap !(UserActorMap ObjectType ActorStatus) !(IntMap Device) !Network !*TagSource
            -> Image (Maps2D, MapAction SectionStatus)
maps2DImage disabledSections act mngmnt ms2d exitLocks hopLocks inventoryMap statusMap sectionUsersMap userActorMap allDevices network tsrc
//TODO  = above [] [] ('DL'.strictTRMap ((margin (px 5.0, px zero)) o (map2DImage disabledSections act mngmnt exitLocks hopLocks inventoryMap statusMap sectionUsersMap userActorMap allDevices network)) (zip2 [0..] ms2d)) NoHost
  = above [] [] Nothing [] ('DL'.strictTRMap ((margin (px 5.0, px zero)) o (map2DImage disabledSections act mngmnt exitLocks hopLocks inventoryMap statusMap sectionUsersMap userActorMap allDevices network)) (zip2 [0..] ms2d)) NoHost

map2DImage :: !(Set Coord3D) !(MapAction SectionStatus) !RenderMode !SectionExitLockMap !SectionHopLockMap !MySectionInventoryMap !MySectionStatusMap !SectionUsersMap !(UserActorMap ObjectType ActorStatus) !(IntMap Device) !Network !(!Maps2DIndex, !Map2D)
           -> Image (Maps2D, MapAction SectionStatus)
map2DImage disabledSections act mngmnt exitLocks hopLocks inventoryMap statusMap sectionUsersMap userActorMap allDevices network (floorIdx, {Map2D | shape2D, doors2D, size2D = size2D=:(w, h), map2D, mapId})
  #! titleImg    = mapTitleImage floorIdx (hilite act) size2D (toString mapId)
  #! sectionsImg = sectionsImage disabledSections act mngmnt exitLocks hopLocks inventoryMap statusMap sectionUsersMap userActorMap allDevices network doors2D size2D floorIdx map2D
  #! lowerImg    = mask sectionsImg size2D shape2D
//TODO  = above [] [] [titleImg, lowerImg] NoHost
  = above [] [] Nothing [] [titleImg, lowerImg] NoHost

mask :: !(Image m) !Size2D !(Maybe Shape2D) -> Image m
mask image _ Nothing
  = image
mask image (w,h) (Just shape)
  //TODO #! shipshape = polygon Nothing [(px x, px y) \\ (x, y) <- shape]
  #! shipshape = polygon [(px x, px y) \\ (x, y) <- shape]
  #! maskshape = overlay [] [] [shipshape <@< {fill = toSVGColor "white"} <@< {stroke = toSVGColor "white"}] (Host (rect (px w) (px h)))
  = overlay [] [] [image, shipshape <@< {fill = toSVGColor "none"} <@< {stroke = toSVGColor "black"}] NoHost <@< {MaskAttr | mask = maskshape}


sectionsImage :: !(Set Coord3D) !(MapAction SectionStatus) !RenderMode !SectionExitLockMap !SectionHopLockMap !MySectionInventoryMap !MySectionStatusMap !SectionUsersMap !(UserActorMap ObjectType ActorStatus) !(IntMap Device) !Network !Size2D !Size2D !Maps2DIndex ![[Section]]
              -> Image (Maps2D, MapAction SectionStatus)
sectionsImage disabledSections act mngmnt exitLocks hopLocks inventoryMap statusMap sectionUsersMap userActorMap allDevices network doors mdims floorIdx sections
  = sectionsImage` mkSectionImage floorIdx mdims sections
  where
  mkSectionImage :: !Real !Real !Section !Int !Int !Int -> Image (Maps2D, MapAction SectionStatus)
  mkSectionImage dx dy cell floorIdx rowIdx colIdx
    #! cellIdx   = (floorIdx, {col = colIdx, row = rowIdx})
    #! inventory = fromMaybe 'DIS'.newMap ('DM'.get cellIdx inventoryMap)
    #! status    = fromMaybe NormalStatus ('DM'.get cellIdx statusMap)
    #! actors    = [a \\ Just us <- ['DM'.get cellIdx sectionUsersMap], u <- us, Just a <- ['DM'.get u userActorMap]]
    = sectionImage disabledSections (hilite act) mngmnt False exitLocks hopLocks inventory status actors allDevices network doors (dx, dy) cell floorIdx rowIdx colIdx

sectionsImage` :: !(Real Real Section Int Int Int -> Image (Maps2D, MapAction SectionStatus)) !Int !Size2D ![[Section]]
              -> Image (Maps2D, MapAction SectionStatus)
sectionsImage` mkSectionImage floorIdx (mwidth, mheight) sections=:[cols : _]
  #! nr_rows     = length sections
  #! nr_cols     = length cols
  #! row_indices = [0..nr_rows - 1]
  #! col_indices = [0..nr_cols - 1]
  #! dx          = mwidth  / toReal nr_cols
  #! dy          = mheight / toReal nr_rows
  = collage [  (px (dx * toReal col), px (dy * toReal row))
            \\ row <- row_indices, col <- col_indices
            ]
            [  mkSectionImage dx dy cell floorIdx rowIdx colIdx
            \\ (rowIdx, row) <- zip2 row_indices sections, (colIdx, cell) <- zip2 col_indices row
            ]
            (Host (empty (px mwidth) (px mheight)))
sectionsImage` _ _ _ _ = text (mapFont mapTitleFontSize) "No sections defined"

sectionImage :: !(Set Coord3D) !(Maybe EditHilite) !RenderMode !Bool !SectionExitLockMap !SectionHopLockMap !MyInventory !SectionStatus !MyActors !(IntMap Device) !Network !Size2D !Size2D !Section !Int !Int !Int
             -> Image (Maps2D, MapAction SectionStatus)
sectionImage disabledSections hilite mngmnt zoomed exitLocks hopLocks inventoryMap statusMap sectionUsersMap allDevices network doorDims sdims section floorIdx rowIdx colIdx
  = sectionImage` (mkRest hilite mngmnt exitLocks statusMap sectionUsersMap sdims section) mngmnt zoomed hopLocks inventoryMap allDevices network doorDims sdims section floorIdx rowIdx colIdx
  where
  mkRest :: !(Maybe EditHilite) !RenderMode !SectionExitLockMap !SectionStatus !MyActors !Size2D !Section !Bool !Coord3D ![MyObject] ![Device] !Real
            !(Image (Maps2D, MapAction SectionStatus)) ![Image b] !(Image (Maps2D, MapAction SectionStatus))
            !(Image (Maps2D, MapAction SectionStatus)) !(Image (Maps2D, MapAction SectionStatus))
            !(Image (Maps2D, MapAction SectionStatus)) !(Image (Maps2D, MapAction SectionStatus)) !(Image (Maps2D, MapAction SectionStatus))
         -> Image (Maps2D, MapAction SectionStatus)
  mkRest hilite mngmnt exitLocks statusMap actorMap (swidth, sheight) {Section | borders={Borders | n, e, s, w},hops} canCloseDoors c3d inventory devices multiplier inventoryBadges deviceBadges cableBadges upDownExits hdoor vdoor hwall vwall
// TODO   #! actorBadges     = above (repeat AtMiddleX) [] ('DL'.strictTRMap (scale multiplier multiplier o mkActorBadge) actorMap) NoHost
    #! actorBadges     = above (repeat AtMiddleX) [] Nothing [] ('DL'.strictTRMap (scale multiplier multiplier o mkActorBadge) actorMap) NoHost
//TODO    #! statusBadges    = above (repeat AtLeft) []
    #! statusBadges    = above (repeat AtLeft) [] Nothing []
                           [ mkStatusBadges statusMap c3d mngmnt multiplier [HasSmallFire, HasMediumFire, HasBigFire]
                           , mkStatusBadges statusMap c3d mngmnt multiplier [HasSmoke]
                           , mkStatusBadges statusMap c3d mngmnt multiplier [HasSomeWater, IsFlooded]
                           ]
                           NoHost
    #! pxswidth        = px swidth
    #! pxsheight       = px sheight
    #! host            = rect pxswidth pxsheight <@< {onclick     = onClick (FocusOnSection c3d), local = False}
                                                 <@< {opacity     = if (hiliteThisSection hilite c3d) 1.0 0.0}
                                                 <@< {stroke      = toSVGColor "none" }
                                                 <@< {strokewidth = px 0.0 }
                                                 <@< {fill        = if (hiliteThisSection hilite c3d) hiliteSectionBackgroundColor (toSVGColor "white")}
    #! host            = if (mngmnt === DOffMode && 'DS'.member c3d disabledSections)
                           (overlay (repeat (AtMiddleX, AtMiddleY)) [] [ host
                                                                       , line /*Nothing Slash */pxswidth pxsheight <@< {stroke = toSVGColor "red" }
                                                                       , line /*Nothing Backslash */pxswidth pxsheight <@< {stroke = toSVGColor "red" }
                                                                       ] NoHost)
                           host
    = overlay [ (AtMiddleX, AtTop), (AtRight, AtMiddleY), (AtMiddleX, AtBottom), (AtLeft, AtMiddleY) // Walls
              , (AtLeft, AtTop), (AtRight, AtTop), (AtLeft, AtBottom), (AtRight, AtBottom) // Badges
              ]
              [ (px 0.0, px 0.0), (px 0.0, px 0.0), (px 0.0, px 0.0), (px 0.0, px 0.0) // Walls
              , (px 3.0, px 3.0), (px -3.0, px 3.0), (px 3.0, px -3.0), (px -6.0, px -3.0) // Badges
              ]
              [ case n of Wall = hwall
                          //TODO Door = above (repeat AtMiddleX) [] [hwall, doorClick canCloseDoors c3d N (hdoor <@< doorFill exitLocks c3d N)] NoHost
                          Door = above (repeat AtMiddleX) [] Nothing [] [hwall, doorClick canCloseDoors c3d N (hdoor <@< doorFill exitLocks c3d N)] NoHost
                          Open = empty zero zero
              , case e of Wall = vwall
                          //TODO Door = beside (repeat AtMiddleY) [] [doorClick canCloseDoors c3d E (vdoor <@< doorFill exitLocks c3d E), vwall] NoHost
                          Door = beside (repeat AtMiddleY) [] Nothing [] [doorClick canCloseDoors c3d E (vdoor <@< doorFill exitLocks c3d E), vwall] NoHost
                          Open = empty zero zero
              , case s of Wall = hwall
                          //TODO Door = above (repeat AtMiddleX) [] [doorClick canCloseDoors c3d S (hdoor <@< doorFill exitLocks c3d S), hwall] NoHost
                          Door = above (repeat AtMiddleX) [] Nothing [] [doorClick canCloseDoors c3d S (hdoor <@< doorFill exitLocks c3d S), hwall] NoHost
                          Open = empty zero zero
              , case w of Wall = vwall
                          //TODO Door = beside (repeat AtMiddleY) [] [vwall, doorClick canCloseDoors c3d W (vdoor <@< doorFill exitLocks c3d W)] NoHost
                          Door = beside (repeat AtMiddleY) [] Nothing [] [vwall, doorClick canCloseDoors c3d W (vdoor <@< doorFill exitLocks c3d W)] NoHost
                          Open = empty zero zero
              , statusBadges, actorBadges, inventoryBadges, upDownExits
              ]
              (Host host)
    where
    mkStatusBadges :: !SectionStatus !Coord3D !RenderMode !Real ![SectionStatus] -> Image (a, MapAction SectionStatus)
   //TODO mkStatusBadges statusMap c3d mngmnt multiplier xs = beside (repeat AtMiddleY) [] ('DL'.reverseTR (foldl (mkStatusBadge statusMap c3d mngmnt multiplier) [] xs)) NoHost
    mkStatusBadges statusMap c3d mngmnt multiplier xs = beside (repeat AtMiddleY) [] Nothing [] ('DL'.reverseTR (foldl (mkStatusBadge statusMap c3d mngmnt multiplier) [] xs)) NoHost

    doorFill :: !SectionExitLockMap !Coord3D !Dir -> FillAttr a
    doorFill exitLocks c3d dir
      #! isLocked = isMember dir (fromMaybe [] ('DM'.get c3d exitLocks))
      = { fill = toSVGColor (if isLocked "black" "white") }
    doorClick :: !Bool !Coord3D !Dir !(Image (a, MapAction SectionStatus)) -> Image (a, MapAction SectionStatus)
    doorClick False c3d dir img = img
    doorClick _     c3d dir img = img <@< { onclick = \_ (x, _) -> (x, ToggleDoor c3d dir), local = False}

sectionImage` :: !(Bool Coord3D [Object ObjectType] [Device] Real (Image a) [Image a] (Image b) (Image (Maps2D, MapAction SectionStatus))
                   (Image d) (Image e) (Image f) (Image g) -> Image (Maps2D, MapAction SectionStatus))
                 !RenderMode !Bool !SectionHopLockMap !MyInventory
                 !(IntMap Device) !Network !Size2D !Size2D !Section !Int !Int !Int
              -> Image (Maps2D, MapAction SectionStatus)
sectionImage` f mngmnt zoomed hopLocks inventoryMap allDevices network (doorw, doord) (swidth, sheight) {Section | borders = {Borders | n, e, s, w}, hops} floorIdx rowIdx colIdx
  #! c3d             = (floorIdx, {col = colIdx, row = rowIdx})
  #! inventory       = 'DIS'.elems inventoryMap
  #! devices         = [ el \\ el <- devicesForSection c3d network allDevices
                       | not (isDetector el.Device.deviceType.DeviceType.kind) ]
  #! multiplier      = if zoomed 2.0 1.0
  #! inventoryBadges = 'DL'.strictTRMap (drawInventory multiplier) inventory
  #! deviceBadges    = 'DL'.strictTRMap (drawDevice c3d multiplier) devices
  #! allBadges       = inventoryBadges ++ deviceBadges
  #! inventoryBadges = if (length allBadges > 0)
                         //TODO (beside (repeat AtMiddleY) [] allBadges NoHost)
                         (beside (repeat AtMiddleY) [] Nothing [] allBadges NoHost)
                         (empty zero zero)
  #! cables          = cablesForSection c3d network
  #! cableBadges     = if (length cables > 0)
                         //TODO (above (repeat AtMiddleX) [] ('DL'.strictTRMap mkCable cables) NoHost)
                         (above (repeat AtMiddleX) [] Nothing [] ('DL'.strictTRMap mkCable cables) NoHost)
                         (empty zero zero)
  #! canCloseDoors   = mngmnt === KitchenMode || mngmnt === DOffMode
  //TODO #! upDownExits     = beside [] [(px -3.0,zero)] ('DL'.strictTRMap (drawHop c3d hopLocks multiplier) hops) NoHost
  #! upDownExits     = beside [] [px -3.0] Nothing [] ('DL'.strictTRMap (drawHop c3d hopLocks multiplier) hops) NoHost
  #! hdoor           = rect (px doorw) (px doord)
  #! vdoor           = rect (px doord) (px doorw)
  #! hwall           = xline (px swidth)
  #! vwall           = yline (px sheight)
  = f canCloseDoors c3d inventory devices multiplier inventoryBadges deviceBadges cableBadges upDownExits hdoor vdoor hwall vwall
  where
  drawInventory :: !Real !(Object ObjectType) -> Image a
  drawInventory multiplier i = scale multiplier multiplier (mkInventoryBadge False True (toString i % (0, 1)))

  drawDevice :: !Coord3D !Real !Device -> Image a
  drawDevice c3d multiplier device = scale multiplier multiplier (mkInventoryBadge (deviceIsDisabledInSection c3d device allDevices network) False (toString device % (0, 1)))

  drawHop :: !Coord3D !SectionHopLockMap !Real !Coord3D -> Image (a, MapAction SectionStatus)
  drawHop c3d hopLocks multiplier hop = scale multiplier multiplier (mkUpDown c3d hop hopLocks) <@< { onclick = onClick (ToggleHop c3d hop), local = False }

  mkCable :: !Cable -> Image a
  mkCable cable
    #! linePiece = xline (px 4.0)
    //TODO = beside (repeat AtMiddleY) [] [linePiece, text (mapFont mapTitleFontSize) (cable.Cable.description % (0, 1)), linePiece] NoHost
    = beside (repeat AtMiddleY) [] Nothing [] [linePiece, text (mapFont mapTitleFontSize) (cable.Cable.description % (0, 1)), linePiece] NoHost

mkUpDown :: !Coord3D !Coord3D !SectionHopLockMap -> Image a
mkUpDown cur=:(curFloor, _) next=:(nextFloor, _) hopLocks
  #! l      = case 'DM'.get cur hopLocks of
                Just xs -> 'DL'.elem next xs
                _       -> False
  #! goesUp = curFloor > nextFloor
 //TODO  = beside (repeat AtBottom) [] ('DL'.strictTRMap (\n -> rect (px 3.0) ((px 3.0) *. n)) (if goesUp [1,2,3] [3,2,1])) NoHost <@< { opacity = if l 0.3 1.0 }
  = beside (repeat AtBottom) [] Nothing [] ('DL'.strictTRMap (\n -> rect (px 3.0) ((px 3.0) *. n)) (if goesUp [1,2,3] [3,2,1])) NoHost <@< { opacity = if l 0.3 1.0 }

mkStatusBadge :: !SectionStatus Coord3D !RenderMode !Real ![Image (a, MapAction SectionStatus)] !SectionStatus
              -> [Image (a, MapAction SectionStatus)]
mkStatusBadge activeSectionStatus c3d mngmnt badgeMult acc roomStatus
  #! high      = activeSectionStatus === roomStatus
  #! isKitchen = mngmnt === KitchenMode
  | high || isKitchen
    #! img = scale badgeMult badgeMult (mkStatusBadgeBackground roomStatus) <@< { opacity = if high 1.0 0.3 }
    #! img = if isKitchen
               (img <@< { onclick = onClick (SetStatus c3d roomStatus), local = False })
               img
    = [img : acc]
  | otherwise = acc

mkStatusBadgeBackground :: !SectionStatus -> Image a
mkStatusBadgeBackground HasSomeWater  = smallBadgeImage <@< { fill = toSVGColor "lightblue" }
mkStatusBadgeBackground IsFlooded     = smallBadgeImage <@< { fill = toSVGColor "royalblue" }
mkStatusBadgeBackground HasSmallFire  = smallBadgeImage <@< { fill = toSVGColor "sandybrown" }
mkStatusBadgeBackground HasMediumFire = smallBadgeImage <@< { fill = toSVGColor "orangered" }
mkStatusBadgeBackground HasBigFire    = smallBadgeImage <@< { fill = toSVGColor "darkred" }
mkStatusBadgeBackground HasSmoke      = smallBadgeImage <@< { fill = toSVGColor "grey" }
mkStatusBadgeBackground _             = smallBadgeImage <@< { fill = toSVGColor "none" }

mkActorBadge :: !MyActor -> Image a
mkActorBadge {actorStatus = {occupied}, userName, carrying}
  #! actorBadge  = mkActorBadgeBackground occupied
  #! userStr     = toString userName
  #! userInitial = text myFontDef (userStr % (0,0)) <@< { fill = toSVGColor "white" }
  #! actorBadge  = overlay [(AtMiddleX, AtMiddleY)] [] [userInitial] (Host actorBadge)
  #! inventory   = 'DL'.strictTRMap (\i -> mkInventoryBadge False True (toString i % (0, 1))) carrying
  //TODO = above (repeat AtMiddleX) [] [actorBadge : inventory] NoHost
  = above (repeat AtMiddleX) [] Nothing [] [actorBadge : inventory] NoHost

mkActorBadgeBackground :: !Availability -> Image a
mkActorBadgeBackground occupied = medBadgeImage <@< { fill = toSVGColor (case occupied of
                                                                        Available    -> "green"
                                                                        NotAvailable -> "black"
                                                                        Busy         -> "orange")}

mkInventoryBadge :: !Bool !Bool !String -> Image b
mkInventoryBadge disabled portable str
  #! txt = text myFontDef str <@< { fill = toSVGColor "white" }
  = overlay [(AtMiddleX, AtMiddleY)] [] [txt] (Host (mkInventoryBadgeBackground disabled portable))

mkInventoryBadgeBackground :: !Bool !Bool -> Image b
mkInventoryBadgeBackground disabled portable
  = wideBadgeImage <@< { fill = toSVGColor (if portable "BlueViolet" "purple") }
                   <@< { stroke = toSVGColor (if disabled "red" "black") }
                   <@< { strokewidth = px (if disabled 2.0 1.0) }

smallBadgeImage :: Image a
smallBadgeImage = rect (px 6.0) (px 6.0) <@< { stroke = toSVGColor "black" }
                                         <@< { strokewidth = px 1.0 }

medBadgeImage :: Image a
medBadgeImage = rect (px 10.0) (px 10.0) <@< { stroke = toSVGColor "black" }
                                         <@< { strokewidth = px 1.0 }

wideBadgeImage :: Image a
wideBadgeImage = rect (px 16.0) (px 11.0) <@< { stroke = toSVGColor "black" }
                                          <@< { strokewidth = px 1.0 }

roomDim =: 64.0
exitWidth =: 16.0

myFontDef = normalFontDef "Arial" 10.0

roomImage :: !Coord3D !SectionExitLockMap !SectionHopLockMap !MyInventory !SectionStatus !MyActors !(IntMap Device) !Network !Bool !Section !Map2D !(MapAction SectionStatus) !*TagSource
          -> Image (Maps2D, MapAction SectionStatus)
roomImage c3d=:(floorIdx, {col, row}) exitLocks hopLocks inventoryMap statusMap actorMap allDevices network zoomed room m2d editlayout tsrc
  = sectionImage 'DS'.newSet Nothing WalkAroundMode True exitLocks hopLocks inventoryMap statusMap actorMap allDevices network m2d.doors2D m2d.size2D room floorIdx row col

:: EditHilite = MapHilite !Maps2DIndex | SectionHilite !Maps2DIndex !Coord2D

hilite :: !(MapAction SectionStatus) -> Maybe EditHilite
hilite (FocusOnMap      idx)	= Just (MapHilite     idx)
hilite (FocusOnSection (idx,c))	= Just (SectionHilite idx c)
hilite _						= Nothing

hiliteThisMap :: !(Maybe EditHilite) !Maps2DIndex -> Bool
hiliteThisMap (Just what) idx`	= case what of
								    MapHilite     idx   = idx == idx`
								    SectionHilite idx _ = idx == idx`
hiliteThisMap _ _				= False

hiliteThisSection :: !(Maybe EditHilite) !Coord3D -> Bool
hiliteThisSection (Just (SectionHilite idx c)) (idx`,c`)
								= idx == idx` && c == c`
hiliteThisSection _	_			= False

editLayoutImage :: !(MapAction SectionStatus) !(IntMap Device) !Network !MySectionInventoryMap !Maps2DIndex !Map2D
                -> Image (Maps2D, MapAction SectionStatus)
editLayoutImage act allDevices network inventoryMap idx {Map2D | shape2D, doors2D, size2D = size2D=:(w, h), map2D, mapId}
  #! titleImg  = mapTitleImage idx (hilite act) size2D (hint mapId) <@< {onclick = onClick (FocusOnMap idx), local = False}
  #! editImg   = editSectionsImage (hilite act) allDevices network inventoryMap idx doors2D size2D map2D
  #! bottomImg = mask editImg size2D shape2D
  = above [] [] Nothing [] [titleImg, bottomImg] NoHost
//TODO  = above [] [] [titleImg, bottomImg] NoHost

hint msg = msg +++ " (click in this area to edit this map)"

devicesForSection :: !Coord3D !Network !(IntMap Device) -> [Device]
devicesForSection c3d network allDevices
  #! deviceIds = fromMaybe [] ('DM'.get c3d network.devices)
  = [dev \\ devId <- deviceIds, Just dev <- ['DIS'.get devId allDevices]]

editSectionsImage :: !(Maybe EditHilite) !(IntMap Device) !Network !MySectionInventoryMap !Int !Size2D !Size2D ![[Section]]
                  -> Image (!Maps2D, !MapAction SectionStatus)
editSectionsImage hilite allDevices network inventoryMap floorIdx doors mdims sections
  = sectionsImage` (mkSectionImage hilite) floorIdx mdims sections
  where
  mkSectionImage :: !(Maybe EditHilite) !Real !Real !Section !Int !Int !Int
                 -> Image (!Maps2D, !MapAction SectionStatus)
  mkSectionImage hilite dx dy cell floorIdx rowIdx colIdx
    #! coord     = {col = colIdx, row = rowIdx}
    #! cellIdx   = (floorIdx, coord)
    #! inventory = fromMaybe 'DIS'.newMap ('DM'.get cellIdx inventoryMap)
    = editSectionImage hilite EditMode False allDevices network inventory doors (dx, dy) coord cell floorIdx rowIdx colIdx

editSectionImage :: !(Maybe EditHilite) !RenderMode !Bool !(IntMap Device) !Network !MyInventory !Size2D !Size2D !Coord2D !Section !Int !Int !Int
                 -> Image (!Maps2D, !MapAction SectionStatus)
editSectionImage hilite mngmnt zoomed allDevices network inventoryMap doorDims sdims=:(width, height) c section floorIdx rowIdx colIdx
  = sectionImage` (mkRest hilite section) mngmnt zoomed 'DM'.newMap inventoryMap allDevices network doorDims sdims section floorIdx rowIdx colIdx
  where
  mkRest :: !(Maybe EditHilite) !Section !Bool !Coord3D ![MyObject] ![Device] !Real
            !(Image (Maps2D, MapAction SectionStatus)) ![Image b] !(Image (Maps2D, MapAction SectionStatus))
            !(Image (Maps2D, MapAction SectionStatus)) !(Image (Maps2D, MapAction SectionStatus))
            !(Image (Maps2D, MapAction SectionStatus)) !(Image (Maps2D, MapAction SectionStatus)) !(Image (Maps2D, MapAction SectionStatus))
         -> Image (Maps2D, MapAction SectionStatus)
  mkRest hilite {Section | borders={Borders | n, e, s, w}} canCloseDoors c3d inventory devices multiplier inventoryBadges deviceBadges cableBadges upDownExits hdoor vdoor hwall vwall
    #! hwally = rect (px width) (px 5.0) <@< { fill    = toSVGColor "white" }
                                         <@< { opacity = 0.1 }
                                         <@< { stroke  = toSVGColor "none" }
    #! vwally = rect (px 5.0) (px height) <@< { fill    = toSVGColor "white" }
                                          <@< { opacity = 0.1 }
                                          <@< { stroke  = toSVGColor "none" }
    #! wallyN = hwally <@< {onclick = rotateWall floorIdx c N, local = False}
    #! wallyE = vwally <@< {onclick = rotateWall floorIdx c E, local = False}
    #! wallyW = vwally <@< {onclick = rotateWall floorIdx c W, local = False}
    #! wallyS = hwally <@< {onclick = rotateWall floorIdx c S, local = False}
    = 'GS'.overlay [ ('GS'.AtMiddleX, 'GS'.AtBottom), ('GS'.AtRight, 'GS'.AtMiddleY), ('GS'.AtMiddleX, 'GS'.AtTop), ('GS'.AtLeft, 'GS'.AtMiddleY)
              , ('GS'.AtLeft, 'GS'.AtBottom), ('GS'.AtRight, 'GS'.AtBottom), ('GS'.AtRight, 'GS'.AtBottom)
              ]
              [ ('GS'.px 0.0, 'GS'.px 0.0), ('GS'.px 0.0, 'GS'.px 0.0), ('GS'.px 0.0, 'GS'.px 0.0), ('GS'.px 0.0, 'GS'.px 0.0) // Walls
              , ('GS'.px 3.0, 'GS'.px -3.0), ('GS'.px -3.0, 'GS'.px -3.0), ('GS'.px -6.0, 'GS'.px -3.0) // Badges, cables
              ]
              [ case s of Wall = 'GS'.overlay (repeat ('GS'.AtMiddleX, 'GS'.AtBottom)) [] [       hwall, wallyS] NoHost
                          Door = 'GS'.overlay (repeat ('GS'.AtMiddleX, 'GS'.AtBottom)) [] [hdoor, hwall, wallyS] NoHost
                          Open = wallyS
              , case e of Wall = 'GS'.overlay (repeat ('GS'.AtRight, 'GS'.AtMiddleY))  [] [       vwall, wallyE] NoHost
                          Door = 'GS'.overlay (repeat ('GS'.AtRight, 'GS'.AtMiddleY))  [] [vdoor, vwall, wallyE] NoHost
                          Open = wallyE
              , case n of Wall = 'GS'.overlay (repeat ('GS'.AtMiddleX, 'GS'.AtTop))    [] [hwall,        wallyN] NoHost
                          Door = 'GS'.overlay (repeat ('GS'.AtMiddleX, 'GS'.AtTop))    [] [hwall, hdoor, wallyN] NoHost
                          Open = wallyN
              , case w of Wall = 'GS'.overlay (repeat ('GS'.AtLeft, 'GS'.AtMiddleY))   [] [vwall,        wallyW] NoHost
                          Door = 'GS'.overlay (repeat ('GS'.AtLeft, 'GS'.AtMiddleY))   [] [vwall, vdoor, wallyW] NoHost
                          Open = wallyW
              , inventoryBadges, cableBadges, upDownExits
              ]
              (Host ('GS'.rect ('GS'.px width) ('GS'.px height) <@< {dash    = [5, 2]} <@< {stroke = editSectionDashColor}
                                                 <@< {fill    = if (hiliteThisSection hilite c3d) hiliteSectionBackgroundColor editSectionBackgroundColor}
                                                 <@< {onclick = onClick (FocusOnSection (floorIdx, c)), local = False}
              )     )

rotateWall :: !Int Coord2D Dir (Span,Span) !(!Maps2D, MapAction SectionStatus)
           -> (!Maps2D, !MapAction SectionStatus)
rotateWall m c d _ (maps, edit)
  = case getMap2D m maps of
      Just map
        = case getSection c map of
            Just s
              #! b` = rotate (getBorder d s)
              #! s` = setBorder d b` s
              = (setMap2D m (updSection (twin d c) (setBorder (opposite d) b`) (setSection c s` map)) maps, edit)
            _ = (maps, edit)
      _ = (maps, edit)
  where
  rotate :: !Border -> Border
  rotate Open = Wall
  rotate Wall = Door
  rotate Door = Open

onClick :: !(MapAction SectionStatus) (Span,Span) !(!a, MapAction SectionStatus) -> (!a, !MapAction SectionStatus)
onClick clck _ (m, _) = (m, clck)
