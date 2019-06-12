implementation module C2.Apps.ShipAdventure.Editor

import iTasks
import iTasks.Extensions.SVG.SVGEditor
import iTasks.Extensions.JSONFile
import iTasks.Internal.IWorld
import iTasks.UI.Layout, iTasks.UI.Definition
import System.Directory, System.File, System.FilePath
import Text
import StdArray, StdFile
import StdMisc
import C2.Framework.MapEnvironment
import C2.Apps.ShipAdventure.Types
import C2.Apps.ShipAdventure.Images
import qualified Data.Map as DM
import Data.Map.GenJSON
import qualified Data.IntMap.Strict as DIS
import qualified Data.Set as DS

import Graphics.Scalable.Image => qualified grid
import Graphics.Scalable.Types
//from Graphics.Scalable import normalFontDef, above, class margin(..), instance margin (Span,Span), px
//from Graphics.Scalable import :: ImageOffset, :: Host(..)

shipEditorTabs		:: Task ()
shipEditorTabs		= allTasks [ viewLayout          <<@ Title "View Ship"
                               , editLayout          <<@ Title "Edit Ship"
                               , editSectionContents <<@ Title "Edit Section Contents"
                               , manageDevices       <<@ Title "Manage Devices"
                               , manageCables        <<@ Title "Manage Cables"
                               , exportShip          <<@ Title "Export"
                               , importShip          <<@ Title "Import"
                               ] <<@ ArrangeWithTabs False @! ()

exportShip :: Task ()
exportShip
  =                Hint "Enter file name" @>> enterInformation []
  >>= \fileName -> get (myInventoryMap |*| myNetwork |*| myCables |*| myDevices |*| maps2DShare)
  >>- \data     -> exportJSONFile (fileName +++ ".map") data
  >>|              Title "Success!" @>> viewInformation [] "File exported"
  >>|              exportShip @! ()

importShip :: Task ()
importShip
  =                getMapNames
  >>= \mapNames -> Hint "Select file" @>> enterChoice [] mapNames
  >>*              [ OnAction (Action "Import") (hasValue doImport)
                   , OnAction (Action "Refresh list") (always importShip)
                   ]
  where
  doImport :: !String -> Task ()
  doImport mapName
    =            getMap mapName
    >>- \data -> set data (myInventoryMap >*< myNetwork >*< myCables >*< myDevices >*< maps2DShare)
    >>|          Title "Ship imported" @>> viewInformation [] "Ship imported"
    >>|          importShip @! ()

getMap :: !String -> Task (!(!(!(!MySectionInventoryMap, !Network), !IntMap Cable), !IntMap Device), !Maps2D)
getMap mapName = get applicationDirectory >>- \curDir -> accWorldError (getMap` mapName curDir) id
  where
  getMap` :: !String !String !*World -> *(!MaybeError String (!(!(!(!MySectionInventoryMap, !Network), !IntMap Cable), !IntMap Device), !Maps2D), !*World)
  getMap` mapName dir world
    # (mjson, world) = readFile (dir </> (mapName +++ ".map")) world
    = case mjson of
        Ok json   -> case fromJSON (fromString json) of
                       Just gg  -> (Ok gg, world)
                       _        -> err ("Failed to deserialize JSON: " +++ json) world
        Error msg -> err (toString msg) world
    where
    err msg world
      # msg = "Failed to load map file " +++ mapName
      = (Error msg, world)

getMapNames :: Task [String]
getMapNames = get applicationDirectory >>- \curDir -> accWorldError (getMapNames curDir) id
  where
  getMapNames :: !String !*World -> *(!MaybeError String [String], !*World)
  getMapNames dir world
    # (mfs, world)  = readDirectory dir world
    = case mfs of
        Ok fs
          = (Ok (map dropExtension (filter (\x -> noDots x && onlyMaps x) fs)), world)
        Error _
          # msg = "Failed to read Tonic directory"
          = (Error  msg, world)
  onlyMaps :: !String -> Bool
  onlyMaps str = endsWith ".map" str

  noDots :: !String -> Bool
  noDots str = not (str.[0] == '.')

mapFont p			= normalFontDef "Verdana" p
mapTitleFontSize    =: 10.0

/********************************************************************************************************************
*
* Viewing the layout of a map
*
********************************************************************************************************************/
viewLayout = updateMapStatus EditMode @! ()

sharedMapAction :: SimpleSDSLens (MapAction SectionStatus)
sharedMapAction = sharedStore "sharedMapAction" (FocusOnMap 0)	// start at the top-level (all maps)

sharedEditShip :: SimpleSDSParallel (Maps2D,MapAction SectionStatus)
sharedEditShip = maps2DShare >*< sharedMapAction

manageDevices :: Task ()
manageDevices
  #! cos = [ChooseFromGrid deviceToEditDevice]
  #! eos = [EnterAs editDeviceToDevice]
  #! vos = [ViewAs deviceToEditDevice]
  #! uos = [UpdateAs deviceToEditDevice (const editDeviceToDevice)]
  = intMapCrudWith "Devices" cos eos vos uos (\dev -> dev.Device.deviceId) myDevices @! ()

deviceToEditDevice :: !Device -> EditDevice
deviceToEditDevice dev
  = { EditDevice
    | description = dev.Device.description
    , deviceType  = deviceTypeToEditDeviceType dev.Device.deviceType
    , deviceId    = dev.Device.deviceId
    , inCables    = dev.Device.inCables
    , outCables   = dev.Device.outCables
    }
  where
  deviceTypeToEditDeviceType :: !DeviceType -> EditDeviceType
  deviceTypeToEditDeviceType devType
    = { EditDeviceType
      | kind     = devType.DeviceType.kind
      , requires = 'DM'.toList devType.DeviceType.requires
      , produces = 'DM'.toList devType.DeviceType.produces
      }


editDeviceToDevice :: !EditDevice -> Device
editDeviceToDevice dev
  = { Device
    | description = dev.EditDevice.description
    , deviceType  = editDeviceTypeToDeviceType dev.EditDevice.deviceType
    , deviceId    = dev.EditDevice.deviceId
    , inCables    = dev.EditDevice.inCables
    , outCables   = dev.EditDevice.outCables
    }
  where
  editDeviceTypeToDeviceType :: !EditDeviceType -> DeviceType
  editDeviceTypeToDeviceType devType
    = { DeviceType
      | kind     = devType.EditDeviceType.kind
      , requires = 'DM'.fromList devType.EditDeviceType.requires
      , produces = 'DM'.fromList devType.EditDeviceType.produces
      }

:: EditDeviceType =
  { kind     ::  !DeviceKind
  , requires ::  ![(!CableType, !Capacity)]
  , produces ::  ![(!CableType, !Capacity)]
  }

:: EditDevice =
  { description ::  !String
  , deviceType  ::  !EditDeviceType
  , deviceId    ::  !DeviceId
  , inCables    ::  ![CableId]
  , outCables   ::  ![CableId]
  }
derive class iTask EditDeviceType, EditDevice

manageCables :: Task ()
manageCables = intMapCrudWith "Cables" [ChooseFromGrid id] [] [] [] (\cable -> cable.Cable.cableId) myCables @! ()

intMapCrud :: !String !(r -> Int) !(SimpleSDSLens (IntMap r)) -> Task r | iTask r
intMapCrud descr mkId share = Title descr @>> crud 'DIS'.elems (putItem mkId) (delItem mkId) share
  where
  putItem :: !(r -> Int) !r !(IntMap r) -> IntMap r
  putItem mkId item allItems = 'DIS'.put (mkId item) item allItems
  delItem :: !(r -> Int) !r !(IntMap r) -> IntMap r
  delItem mkId item allItems = 'DIS'.del (mkId item) allItems

intMapCrudWith :: !String ![ChoiceOption r] [EnterOption r] [ViewOption r] [UpdateOption r] !(r -> Int) !(SimpleSDSLens (IntMap r)) -> Task r | iTask r
intMapCrudWith descr cos eos vos uos mkId share = Title descr @>> crudWith cos eos vos uos 'DIS'.elems (putItem mkId) (delItem mkId) share
  where
  putItem :: !(r -> Int) !r !(IntMap r) -> IntMap r
  putItem mkId item allItems = 'DIS'.put (mkId item) item allItems
  delItem :: !(r -> Int) !r !(IntMap r) -> IntMap r
  delItem mkId item allItems = 'DIS'.del (mkId item) allItems

graphicalMapEditor :: Task ()
graphicalMapEditor
  = Title "Graphical map editor" @>> updateSharedInformation 
      [UpdateSharedUsing id (const fst) const imageEditor]
      (sharedEditShip >*| (myInventoryMap |*| myNetwork |*| myDevices)) @! ()
	  @! ()
where
	imageEditor = fromSVGEditor
		{ initView       = fst
		, renderImage    = \((_, act), ((inventoryMap, network), allDevices)) (ms2d, _) _ ->
		//TODO	above [] [] [margin (px 5.0, px zero) (editLayoutImage act allDevices network inventoryMap idx m2d) \\ m2d <- ms2d & idx <- [0..]] NoHost
			above [] [] Nothing [] [margin (px 5.0, px zero) (editLayoutImage act allDevices network inventoryMap idx m2d) \\ m2d <- ms2d & idx <- [0..]] NoHost
		, updModel       = \(_,data) newClSt -> (newClSt,data)
		}

editLayout :: Task ()
editLayout
  = allTasks [ graphicalMapEditor
             , Title "Edit map dimensions" @>> updateSharedInformation  [UpdateSharedAs toMapsForm fromMapsForm const] maps2DShare @! ()
             , Title "Edit map" @>> updateSharedInformation [UpdateSharedAs toMapActionForm fromMapActionForm const] sharedEditShip @! ()
             , (watch maps2DShare
               -&&- (Title "Quick borders" @>> enterChoiceWithShared [] (mapRead (\ship -> [mapId \\ {Map2D | mapId} <- ship]) maps2DShare))
               >>* [ OnAction (Action "Add outer borders"    ) (hasValue (uncurry (editOuterBorders Wall)))
                   , OnAction (Action "Remove outer borders" ) (hasValue (uncurry (editOuterBorders Open)))
                   ]
               ) @! ()
             ] @! ()//TODO <<@ ApplyLayout layout @! ()
/*
where
	layout = idLayout
	layout = sequenceLayouts
		[ insertSubAt [1] (ui UIContainer) // Group the 'tool' tasks
		, moveSubAt[2] [1,0]
		, moveSubAt[2] [1,1]
		, moveSubAt[2] [1,2]
		, arrangeWithSideBar 1 LeftSide False //Move the 'tool' tasks to the side
		]
*/

editOuterBorders :: !Border !Maps2D !MapID -> Task ()
editOuterBorders border ship mapID = case getMap2DIndex mapID ship of
                                         Just idx = set (updMap2D idx (editBorders border) ship) maps2DShare @! ()
                                         _        = return ()
    where
    editBorders :: !Border !Map2D -> Map2D
    editBorders border m2d=:{map2D = rows=:[cols : _]}
      #! lastRowIdx = length rows - 1
      #! lastColIdx = length cols - 1
      #! m2d = foldr (\colIdx -> updSection {row = 0,          col = colIdx}     (editBorder N border)) m2d [0..lastColIdx]
      #! m2d = foldr (\rowIdx -> updSection {row = rowIdx,     col = 0}          (editBorder W border)) m2d [0..lastRowIdx]
      #! m2d = foldr (\colIdx -> updSection {row = lastRowIdx, col = colIdx}     (editBorder S border)) m2d [0..lastColIdx]
      #! m2d = foldr (\rowIdx -> updSection {row = rowIdx,     col = lastColIdx} (editBorder E border)) m2d [0..lastRowIdx]
      = m2d
    editBorders _ m2d = m2d

    editBorder :: !Dir !Border !Section -> Section
    editBorder dir border s = {Section | s & borders = edit dir border s.Section.borders}

    edit :: !Dir !Border !Borders -> Borders
    edit N border b = {Borders | b & n = border}
    edit E border b = {Borders | b & e = border}
    edit W border b = {Borders | b & w = border}
    edit S border b = {Borders | b & s = border}

editSectionContents :: Task ()
editSectionContents
  = allTasks [ graphicalMapEditor
             , withSelectedSection (
                 \mid c2d -> (Title (mkDesc mid c2d "Inventory"))  @>> updateSharedInformation [UpdateSharedAs fromInv toInv const] (sdsFocus (mid, c2d) inventoryInSectionShare)
               )
             , withSelectedSection (
                 \mid c2d -> let focusedShare = sdsFocus (mid, c2d) devicesInSectionShare
                             in  updateSectionEditor (mkDesc mid c2d "Devices")
                                   [ChooseFromCheckGroup (\d -> d.Device.description)]
                                   (mapRead mrf (myDevices |*< focusedShare)) focusedShare
               )
             , withSelectedSection (
                 \mid c2d -> let focusedShare = sdsFocus (mid, c2d) cablesInSectionShare
                             in  updateSectionEditor (mkDesc mid c2d "Cables")
                                   [ChooseFromCheckGroup (\d -> d.Cable.description)]
                                   (mapRead ('DIS'.elems o fst) (myCables |*< focusedShare)) focusedShare
               )
             ] @! () //TODO <<@ ApplyLayout layout @! ()
  where
  updateSectionEditor :: !String ![ChoiceOption a] (Shared sds1 [a]) (Shared sds2 [a]) -> Task [a] | iTask a & RWShared sds1 & RWShared sds2
  updateSectionEditor d updOpts listShare focusedShare
	= Title d @>> editSharedMultipleChoiceWithShared updOpts listShare focusedShare

  withSelectedSection :: !(Int Coord2D -> Task a) -> Task () | iTask a
  withSelectedSection f
    = whileUnchanged sharedMapAction
        (\editLayout -> case editLayout of
           FocusOnSection (mid, c2d) = f mid c2d @! ()
           _                         = (Title "Please select section") @>> viewInformation [] "Please select section" @! ()
        )

/*
  layout = idLayout
  layout = sequenceLayouts
		[insertSubAt [1] (uia UIContainer (directionAttr Horizontal))
		,moveSubAt [2] [1,0]
		,moveSubAt [2] [1,1]
		,moveSubAt [2] [1,2]
		,arrangeWithSideBar 1 BottomSide False
		]
*/

  mkDesc :: !Int !Coord2D !String -> String
  mkDesc mid c2d str = str +++ " in section " +++ toString c2d +++ " on deck " +++ toString mid

  mrf :: !(!IntMap Device, a) -> [Device]
  mrf (ds, _) = filter (\d -> not (isDetector d.Device.deviceType.DeviceType.kind)) ('DIS'.elems ds)

  fromInv :: !(IntMap MyObject) -> [ObjectType]
  fromInv inv = map (\item -> item.objType) ('DIS'.elems inv)

  toInv :: a ![ObjectType] -> IntMap MyObject
  toInv _ tfd = 'DIS'.fromList (map (\(idx, item) -> (idx, { Object | objId = idx, objType = item})) (zip2 [0..] tfd))

:: EditMaps =
  { noOfMaps    :: !Int  // the number of maps    (> 0)
  , noOfRows    :: !Int  // the number of rows    (> 0)
  , noOfColumns :: !Int  // the number of columns (> 0)
  , mapWidth    :: !Real // the width of the map  (> 0.0)
  , mapHeight   :: !Real // the height of the map (> 0.0)
  , doorWidth   :: !Real // the width of drawn doors
  , doorDepth   :: !Real // the depth of drawn doors
  }
:: EditForm =
  { map2DIndex  :: !Maps2DIndex            // the index position of this map within Maps2D
  , mapId       :: !MapID                  // the identification of the map containing this section
  , newMapId    :: !MapID                  // new identification of this map
  , section     :: !Coord2D                // the unique identification of the section
  , sectionName :: !Maybe String           // descriptive name, need not be unique
  , up          :: !Bool                   // you can go down (except bottom floor)
  , down        :: !Bool                   // you can go up   (except top floor)
  , outline     :: !Maybe Shape2D
  }
derive class iTask EditMaps, EditForm

toMapsForm :: !Maps2D -> EditMaps
toMapsForm []
  = { EditMaps | noOfMaps    = 1
	           , noOfRows    = 1
	           , noOfColumns = 1
	           , mapWidth    = fst initMap2DSize
	           , mapHeight   = snd initMap2DSize
	           , doorWidth   = fst initDoors2DSize
	           , doorDepth   = snd initDoors2DSize
	  }
toMapsForm maps
  = { EditMaps | noOfMaps    = length maps
               , noOfRows    = rows
               , noOfColumns = columns
               , mapWidth    = w
               , mapHeight   = h
               , doorWidth   = dw
               , doorDepth   = dh
      }
where
	(columns,rows) = dimension (hd maps)
	(w, h )        = (hd maps).size2D
	(dw,dh)        = (hd maps).doors2D

fromMapsForm :: !Maps2D !EditMaps -> Maps2D
fromMapsForm maps mapE=:{EditMaps | noOfMaps, noOfRows, noOfColumns, mapWidth, mapHeight, doorWidth, doorDepth}
  = updateNoOfMaps noOfMaps (map updateMap2D maps)
where
	newNoOfMaps		= max 1 noOfMaps
	newNoOfRows		= max 1 noOfRows
	newNoOfCols		= max 1 noOfColumns
	newMapWidth		= max 10.0 mapWidth
	newMapHeight	= max 10.0 mapHeight
	newColWidth		= newMapWidth  / toReal newNoOfCols
	newRowHeight	= newMapHeight / toReal newNoOfRows
	newDoorWidth	= minList [newColWidth,newRowHeight,max 1.0 doorWidth]
	newDoorDepth	= minList [newColWidth,newRowHeight,max 1.0 doorDepth]
	updateMap2D map	= {Map2D | map & map2D   = updateSections newNoOfRows newNoOfCols map.Map2D.map2D
	                               , size2D  = (newMapWidth, newMapHeight)
	                               , doors2D = (newDoorWidth,newDoorDepth)
                                 , shape2D = Just [(dx*newMapWidth,dy*newMapHeight) \\ (dx,dy) <- frigate_outline]
	                  }

	updateSections :: !Int !Int ![[Section]] -> [[Section]]
	updateSections newNoOfRows newNoOfColumns sections
						= widen (heighten sections)
	where
		curNoOfRows		= length sections
		curNoOfColumns	= length (hd sections)

	    heighten :: ![[Section]] -> [[Section]]
		heighten sects	= if (newNoOfRows < curNoOfRows) (take newNoOfRows sects)
						 (if (newNoOfRows > curNoOfRows) (sects ++ [  [  initSection
						                                              \\ col_no <- [0..curNoOfColumns-1]
						                                              ]
						                                           \\ row_no <- [curNoOfRows .. newNoOfRows-1]
						                                           ])
						                                 sects)
	    widen :: ![[Section]] -> [[Section]]
		widen    sects	= if (newNoOfColumns < curNoOfColumns) (map (take newNoOfColumns) sects)
						 (if (newNoOfColumns > curNoOfColumns) [  row ++ [  initSection
						                                                 \\ col_no <- [curNoOfColumns..newNoOfColumns-1]
						                                                 ]
						                                       \\ row <- sects & row_no <- [0..]
						                                       ]
						                                       sects)

	updateNoOfMaps :: !Int !Maps2D -> Maps2D
	updateNoOfMaps newNoOfMaps maps
	| newNoOfMaps <= 0	= maps
	| newNoOfMaps < curNoOfMaps
						= updMap2D topfloor (updSections noUpStairs) (take newNoOfMaps maps)
	| newNoOfMaps > curNoOfMaps
						= maps ++ [initMap2D dim mapsize doorsize ("new map " +++ toString no) \\ no <- [curNoOfMaps+1 .. newNoOfMaps]]
	| otherwise			= maps
	where
		curNoOfMaps		= length maps
		topfloor		= newNoOfMaps-1
		(dim,mapsize,doorsize)
						= if (isEmpty maps) ((1,1),initMap2DSize,initDoors2DSize)
						     (let map = hd maps in (dimension map,map.Map2D.size2D,map.Map2D.doors2D))

		noUpStairs :: !Section -> Section
		noUpStairs s=:{Section | hops}
						= {Section | s & hops = filter (\(idx,_) -> idx <= topfloor) hops}

toMapActionForm :: !(!Maps2D, !MapAction SectionStatus) -> EditForm
toMapActionForm ([], _) = defaultEditForm
toMapActionForm (maps, FocusOnMap idx)
  = toMapActionForm (maps, FocusOnSection (idx, zero))
toMapActionForm (maps, FocusOnSection c3d=:(0, {col = 0, row = 0}))
  = case getSectionFromMap c3d maps of
      Just section = editFormFromSection maps c3d section
      _            = defaultEditForm
toMapActionForm (maps, FocusOnSection (idx, c2d))
  = case getMap2D idx maps of
      Nothing = toMapActionForm (maps, FocusOnSection (0, zero))
      Just map2D
        = case getSection c2d map2D of
            Just section = editFormFromSection maps (idx, c2d) section
            Nothing      = toMapActionForm (maps, FocusOnSection (idx, zero))
toMapActionForm (maps, _) = toMapActionForm (maps, FocusOnSection (0, zero))

editFormFromSection maps (idx, c2d) {Section | sectionName, hops}
  # mapID       = fromJust (getMapID idx maps)
  # bottomFloor = length maps - 1
  = { EditForm | map2DIndex  = idx
               , mapId       = mapID
               , newMapId    = mapID
               , section     = c2d
               , sectionName = if (sectionName == "") Nothing (Just sectionName)
               , up          = (isMember (idx-1, c2d) hops)
               , down        = (isMember (idx+1, c2d) hops)
               , outline     = Nothing
               }

defaultEditForm = { EditForm | map2DIndex  = 0
                             , mapId       = "ERROR"
                             , newMapId    = "ERROR"
                             , section     = {col = 0, row = 0}
                             , sectionName = Nothing
                             , up          = False
                             , down        = False
                             , outline     = Nothing
                             }


fromMapActionForm :: !(!Maps2D, !MapAction SectionStatus) !EditForm -> (!Maps2D, !MapAction SectionStatus)
//fromMapActionForm m v = m
fromMapActionForm (maps,edit) sectionE=:{EditForm | map2DIndex = idx,section=c,newMapId,up,down}
  = (updMap2D idx ((updateMapID newMapId) o (updSection c (updateSection (idx,c) sectionE))) (updHops (idx,c) up down maps),updateSelection (idx,c) edit)
where
	new_hops	= if up [(idx-1,c)] [] ++ if down [(idx+1,c)] []

	updHops :: !Coord3D !Bool !Bool !Maps2D -> Maps2D
	updHops source=:(idx,c) up down maps
	  = case getMap2D idx maps of
	  	  Just map = case getSection c map of
	  	               Just {Section | hops=old_hops}
                         # deleted_hops = removeMembers old_hops new_hops
	  	                 # added_hops   = removeMembers new_hops old_hops
	  	                 = foldl (removeHop source) (foldl (addHop source) maps added_hops) deleted_hops
	  	               _ = maps
	  	  nope = maps
	where
		removeHop c maps (idx,c`) = updMap2D idx (updSection c` (\s=:{Section | hops} -> {Section | s & hops = removeMember c hops})) maps
		addHop c maps (idx,c`) = updMap2D idx (updSection c` (\s=:{Section | hops} -> {Section | s & hops = [c:hops]})) maps

	updateMapID :: !MapID !Map2D -> Map2D
	updateMapID newId map = {Map2D | map & mapId = newId}

	updateSection :: !Coord3D !EditForm !Section -> Section
	updateSection (idx,c) {EditForm | sectionName,up,down} s
		= {Section | s & sectionName=fromMaybe "" sectionName,hops = new_hops}

	updateSelection :: !Coord3D !(MapAction s) -> MapAction s
	updateSelection c3d (FocusOnMap     _)	= FocusOnSection c3d
	updateSelection c3d (FocusOnSection _)	= FocusOnSection c3d
	updateSelection _   action				= action
fromMapActionForm x _ = x

initMap2DSize		=: (100.0, 100.0)
initDoors2DSize     =: ( 10.0,   3.0)
initMap2D (cols,rows) mapsize=:(w,h) doorsize id
					= {Map2D   | mapId       = id
					           , map2D       = [[initSection \\ _ <- [1..cols]] \\ _ <- [1..rows]]
					           , size2D      = mapsize
					           , doors2D     = doorsize
					           , shape2D     = Just [(dx*w,dy*h) \\ (dx,dy) <- frigate_outline]
					  }
initSection    		= {Section | sectionName = ""
					           , borders     = initBorders
					           , hops        = []
					   }
initBorders			= {Borders|n=Open,e=Open,s=Open,w=Open}
frigate_outline		=: [(0.0,0.5)] ++ port ++ [(1.0,0.5)] ++ starboard
where
	port			= [(0.006,0.048),(0.107,0.01),(0.179,0.0),(0.684,0.0),(0.719,0.01),(0.752,0.029),(0.787,0.067),(0.829,0.106),(0.852,0.135),(0.898,0.212),(0.926,0.279),(0.999,0.462)]
	starboard		= [(dx,1.0-dy) \\ (dx,dy) <- reverse port]
