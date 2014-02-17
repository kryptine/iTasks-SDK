module GeoExample

import StdEnv, PView, Derived
import Data.Void, Data.Error, Data.Func, Data.Either, Text.JSON
from Data.List import splitWith

//GEOGRAPHIC EXAMPLE

// In local memory share
:: GeoPerspective =
    { center :: (Int,Int)
    , bounds :: Maybe (Int,Int,Int,Int)
    }

// Used in a task
:: GeoMap =
    { perspective :: GeoPerspective
    , markers :: [(String,(Int,Int))]
    }

// Lists in database
:: Contact =
    { name     :: String
    , type     :: String
    , position :: (Int,Int)
    }

inBounds :: (Int,Int,Int,Int) Contact -> Bool
inBounds (minx,miny,maxx,maxy) {position=(x,y)}
    = x >= minx && x <= maxx && y >= miny && y <= maxy

contactMarker :: Contact -> (String,(Int,Int))
contactMarker {Contact|name,type,position} = (name+++" ("+++type+++")",position)

derive JSONEncode GeoPerspective,GeoMap,Contact
derive JSONDecode GeoPerspective,GeoMap,Contact

allShips :: PView Void [Contact] [Contact] MyWorld
allShips = createStoreView "allShips" ships
where
    ships =
        [{name = "SA",type = "Ship", position = (1,1)}
        ,{name = "SB",type = "Ship", position = (2,3)}
        ,{name = "SC",type = "Ship", position = (3,2)}
        ]

shipByName :: PView String Contact Contact MyWorld
shipByName = applyLens (applySplit allShips {sget = sget`, sput = sput`} tr1) singletonLens
where
    sget` name ships = [s \\ s <- ships | s.Contact.name == name]
    sput` name ships new = (new ++ [s \\ s <- ships | s.Contact.name <> name], (==) name)

shipsByBounds :: PView (Int,Int,Int,Int) [Contact] [Contact] MyWorld
shipsByBounds = applySplit allShips {sget = sget`, sput = sput`} tr1
where
    sget` bounds is = filter (inBounds bounds) is
    sput` bounds is ws
        = let (ds,us) = splitWith (inBounds bounds) is
          in (us ++ ws, notifyFun (ds ++ ws))

    notifyFun ws bounds = any (inBounds bounds) ws

allPlanes :: PView Void [Contact] [Contact] MyWorld
allPlanes = createStoreView "allPlanes" planes
where
    planes =
        [{name = "PA",type = "Plane", position = (2,5)}
        ,{name = "PB",type = "Plane", position = (2,2)}
        ,{name = "PC",type = "Plane", position = (6,6)}
        ]

planeByName :: PView String Contact Contact MyWorld
planeByName = applyLens (applySplit allPlanes {sget = sget`, sput = sput`} tr1) singletonLens
where
    sget` name planes = [p \\ p <- planes | p.Contact.name == name]
    sput` name planes new = (new ++ [p \\ p <- planes | p.Contact.name <> name], (==) name)

planesByBounds :: PView (Int,Int,Int,Int) [Contact] [Contact] MyWorld
planesByBounds = applySplit allPlanes {sget = sget`, sput = sput`} tr1
where
    sget` bounds is = filter (inBounds bounds) is
    sput` bounds is ws
        = let (ds,us) = splitWith (inBounds bounds) is
          in (us ++ ws, notifyFun (ds ++ ws))

    notifyFun ws bounds = any (inBounds bounds) ws

contactsByBounds :: PView (Int,Int,Int,Int) [Contact] [Contact] MyWorld
contactsByBounds = joinLists shipsByBounds planesByBounds splitter
where
    splitter c=:{Contact|type} = (type == "Ship")

makeMapView :: (PView Void GeoPerspective GeoPerspective MyWorld)
            -> (PView Void GeoMap GeoPerspective MyWorld)
makeMapView perspective = pseq perspective contacts paramF writeF readF
where
    paramF {GeoPerspective|bounds} = bounds
    writeF perspective = (perspective,Void)
    readF perspective contacts = {GeoMap|perspective=perspective,markers=map contactMarker contacts}

    contacts = applyLens (maybeParam [] contactsByBounds) readOnlyLens

Start world
	# myworld = createMyWorld world
	
    # (p1,myworld) = createMemoryView {center=(3,3),bounds=Just (2,3,3,6)} myworld

	# myworld = registerForNotification (makeMapView p1) Void "p1"  myworld	

	# (val, myworld) = get (fixP planeByName "PB") myworld
	# (val, myworld) = get (fixP planesByBounds (2,3,3,6)) myworld

//	# (_,myworld) = put p1 {center=(3,3),bounds=Just (2,3,3,6)} myworld
//	# (_,myworld) = put (makeMapView p1) {center=(3,3),bounds=Just (2,3,3,6)} myworld
// 	# (_,myworld) = put (fixP planeByName "PA") {name = "PA",type = "Plane", position = (3,5)} myworld

 	# myworld = registerForNotification planeByName "PC" "PC"  myworld	
 	# (_,myworld) = put (fixP planeByName "PC") {name = "PC",type = "Plane", position = (7,7)} myworld
	# (val, myworld) = get (fixP planeByName "PC") myworld

//	# (val, myworld) = get (makeMapView p1) myworld

	= (val, getWorld myworld)
	
	
	
	
	
	
	
