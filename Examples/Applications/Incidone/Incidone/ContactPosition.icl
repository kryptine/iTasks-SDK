implementation module Incidone.ContactPosition
import iTasks, iTasks.UI.Editor, iTasks.UI.Definition
import qualified Data.Map as DM
import Data.Functor, Text
import qualified Text.Parsers.ParsersKernel as PK
import qualified Text.Parsers.ParsersDerived as PD
import qualified Control.Applicative as CA
from Control.Applicative import class Alternative, class Applicative
from Text.Parsers.ParsersKernel import :: Parser, instance Alternative Parser, instance Applicative Parser, instance Functor Parser

import Incidone.OP.Concepts
import Incidone.Util.TaskPatterns

derive JSONEncode ContactPosition
derive JSONDecode ContactPosition

gEditor{|ContactPosition|} = {Editor|genUI=genUI,updUI=updUI,onEdit=onEdit}
where
	genUI path val mask vst=:{VSt|taskId,optional,disabled}
    	| disabled
			# attr = 'DM'.unions [optionalAttr optional,valueAttr (toJSON (toSingleLineText val))]
        	= (Ok (uia UIViewString attr), vst)
    	# value = case val of
        	PositionDescription s _ = JSONString s
        	PositionLatLng l        = JSONString (formatLatLng l)
		# attr = 'DM'.unions [optionalAttr optional, stdAttributes "position" optional mask
							 ,editAttrs taskId (editorId path) (Just (toJSON value))]
    	= (Ok (uia UIEditString attr), vst)

	updUI dp old om new nm vst
		= (Ok (if (old === new) NoChange (ChangeUI [SetAttribute "value" (toJSON new)] [])),vst)

	onEdit [] JSONNull val _ ust = (PositionDescription "" Nothing,Blanked,ust)
	onEdit [] (JSONString nval) _ _ ust = (parsePosition nval, Touched, ust)
	onEdit dp e val mask ust = (val,mask,ust)

gVerify{|ContactPosition|} {VerifyOptions|optional=False} (_,Blanked)   = MissingValue
gVerify{|ContactPosition|} _ (PositionDescription _ Nothing,mask)       = WarningValue "This position can not be plotted on a map"
gVerify{|ContactPosition|} _ _                                          = CorrectValue Nothing

gText{|ContactPosition|} _ val = [maybe "" printPosition val]

derive gDefault ContactPosition
//derive gEq ContactPosition
gEq{|ContactPosition|} (PositionDescription xlabel Nothing) (PositionDescription ylabel Nothing) = xlabel == ylabel
gEq{|ContactPosition|} (PositionDescription xlabel (Just (xla,xlo))) (PositionDescription ylabel (Just (yla,ylo)))
    = (xlabel == ylabel) && (toString xla == toString yla) && (toString xlo == toString ylo)
gEq{|ContactPosition|} (PositionLatLng (xla,xlo)) (PositionLatLng (yla,ylo))
    = (toString xla == toString yla) && (toString xlo == toString ylo)
gEq{|ContactPosition|} _ _ = False

//Print and parse helper functions
printPosition :: ContactPosition -> String
printPosition (PositionDescription s Nothing)   = s
printPosition (PositionDescription _ (Just l))  = formatLatLng l
printPosition (PositionLatLng l)                = formatLatLng l

formatLatLng (lat,lng) = formatLat lat +++ " " +++ formatLng lng
where
    formatLat lat = if (lat >= 0.0) (formatReal 5 lat +++ "N") (formatReal 5 (~lat) +++ "S")
    formatLng lng = if (lng >= 0.0) (formatReal 5 lng +++ "E") (formatReal 5 (~lng) +++ "W")
    formatReal numDec r = s % (0,slen - numDec - 1) +++ "." +++ s % (slen - numDec,slen)
    where
        s    = toString (toInt (r * toReal (10 ^ numDec)))
        slen = textSize s

parsePosition :: String -> ContactPosition
parsePosition s = case 'PK'.parse parseLatLng (fromString s) "input" "character" of
    'PK'.Succ [(lat,lng)]   = PositionDescription s (Just (lat,lng))
    _                       = PositionDescription s Nothing
where
    parseLatLng = (parseLat 'PD'. <& separator) 'PD'. <&&> (parseLng 'PD'. <& 'PK'.epsilon)
    parseLat    = ((parseDegMinSec 'PK'. <!> parseDegDecimal) 'PD'. <&&> northSouth) 'PD'. <@ (\(lat,f) -> f lat)
    parseLng    = ((parseDegMinSec 'PK'. <!> parseDegDecimal) 'PD'. <&&> eastWest) 'PD'. <@ (\(lng,f) -> f lng)

    //Degrees in deg/min/sec notiation  (e.g 5o23'23'')
    parseDegMinSec  = parseDegrees 'PD'. <&&> parseMinutes 'PD'. <&&> parseSeconds 'PD'. <@ (\(d,(m,s)) -> d + (m / 60.0) + (s / 3600.0))
    parseDegDecimal = parseFloat //Degrees in decimal notitation (e.g 5.423123)

    parseDegrees = nums 'PD'. <& 'PK'.token ['o'] 'PD'. <@ (toReal o toString)
    parseMinutes = nums 'PD'. <& 'PK'.token ['\''] 'PD'. <@ (toReal o toString)
    parseSeconds = nums 'PD'. <& 'PK'.token ['\'','\''] 'PD'. <@ (toReal o toString)

    northSouth  = ('PK'.symbol 'N' 'PD'. <@ const id) 'PK'. <!> ('PK'.symbol 'S' 'PD'. <@ const (~)) //'PK'. <!> ('PK'.yield id)
    eastWest    = ('PK'.symbol 'E' 'PD'. <@ const id) 'PK'. <!> ('PK'.symbol 'W' 'PD'. <@ const (~)) //'PK'. <!> ('PK'.yield id)
    whitespace  = 'PD'. <!+> ('PK'.satisfy isSpace)
    separator   = ('PK'.token [',']) 'PK'. <!> whitespace

    parseFloat  = (nums 'PD'. <&&> frac) 'PD'. <@ (\(n,f) -> toReal (toString (n++f)))
    frac        = ('PK'.symbol '.' 'PD'. <:&> nums) 'PK'. <!> 'PK'.yield []
    nums        = 'PD'. <!+> ('PK'.satisfy isDigit)

latLng :: ContactPosition -> Maybe (Real,Real)
latLng (PositionLatLng latlng)            = Just latlng
latLng (PositionDescription _ mblatlng)   = mblatlng

withinBounds :: ContactBounds ContactPosition -> Bool
withinBounds ((swlat,swlng),(nelat,nelng)) pos = case latLng pos of
    Nothing = False
    Just (lat,lng) = lat >= swlat && lat <= nelat && lng >= swlng && lng <= nelng

derive class iTask ContactMap, ContactMapLayer, ContactMapLayerDefinition, ContactMapMarker, ContactMapRegion, ContactMapMarkerType, ContactTrack

//Crazy uncomparable reals...
derive JSONEncode ContactMapPerspective
derive JSONDecode ContactMapPerspective
derive gEditor ContactMapPerspective
derive gVerify ContactMapPerspective
derive gText ContactMapPerspective

gEq{|ContactMapPerspective|} {ContactMapPerspective|center=(xla,xlo),zoom=xz,bounds=xb} {ContactMapPerspective|center=(yla,ylo),zoom=yz,bounds=yb}
    = (toString xla == toString yla) && (toString xlo == toString ylo) && (xz == yz) && eqBounds xb yb
where
    eqBounds Nothing Nothing = True
    eqBounds (Just ((x1,x2),(x3,x4))) (Just ((y1,y2),(y3,y4)))
        = (toString x1 == toString y1) && (toString x2 == toString y2) && (toString x3 == toString y3) && (toString x4 == toString y4)
    eqBounds _ _ = False

gDefault{|ContactMapPerspective|}
    =  {ContactMapPerspective|center = (52.948300, 4.776007), zoom = 7, cursor = Nothing, bounds = Nothing} //(Full coast centered on Den Helder)


contactToMapMarker :: Bool Bool Contact -> ContactMapMarker
contactToMapMarker ais selected contact=:{Contact|contactNo,name,needsHelp,providesHelp,position,heading,track}
    = contactMapMarker ais selected contactNo name position heading track needsHelp providesHelp

contactGeoToMapMarker :: Bool Bool ContactGeo -> ContactMapMarker
contactGeoToMapMarker ais selected contact=:{ContactGeo|contactNo,name,needsHelp,providesHelp,position,heading,track}
    = contactMapMarker ais selected contactNo name position heading track needsHelp providesHelp

contactMapMarker ais selected contactNo name position heading track needsHelp providesHelp
    = {ContactMapMarker
      |markerId = (if ais "a" "c") +++ toString contactNo
      ,title    = name
      ,position = fromJust position
      ,heading  = fmap (\(Degrees h) -> h) heading
      ,type     = Just (type ais needsHelp providesHelp)
      ,selected = selected
      ,track    = if selected track Nothing
      }
where
    type True _ _ = CMAIS
    type _ True _ = CMNeedsHelp
    type _ _ True = CMUnit
    type _ _ _    = CMOther

cat CMAIS       = 0
cat CMUnit      = 1
cat CMNeedsHelp = 4
cat CMOther     = 3

hasLatLng :: ContactPosition -> Bool
hasLatLng (PositionLatLng _) = True
hasLatLng (PositionDescription _ (Just _)) = True
hasLatLng _ = False

toLeafletMap :: ContactMap -> LeafletMap
toLeafletMap {ContactMap|perspective,layers}
    = {LeafletMap|perspective = toLeafletPerspective perspective
      ,icons = [icon i \\ i <- [1..250]]
      ,layers = map toLeafletLayer layers
      }
where
    convMarkers markers = [conv m \\ m=:{ContactMapMarker|position} <- markers | hasLatLng position]
    conv {ContactMapMarker|markerId,title,position,heading,type,selected}
        = Marker {LeafletMarker|markerId = markerId, title = title, position = pos position, icon = fmap (\t -> iconIndex heading t selected) type, selected = selected}

    pos (PositionLatLng (lat,lng)) = {LeafletLatLng|lat=lat,lng=lng}
    pos (PositionDescription _ (Just(lat,lng))) = {LeafletLatLng|lat=lat,lng=lng}

	icon i = {LeafletIcon|iconUrl ="/ship-icons/"+++toString i+++".png",iconSize=(24,24)}
    iconIndex heading type selected = (cat type + ( (maybe 24 (\d -> d / 15) heading) + (if selected 25 0)) * 5)

toLeafletLayer :: ContactMapLayer -> LeafletLayer
toLeafletLayer {ContactMapLayer|def=CMTileLayer url} = TileLayer url
toLeafletLayer {ContactMapLayer|def=CMRegionsLayer regions}
    = ObjectLayer
        [Polygon {polygonId=regionId
                 ,points = [{LeafletLatLng|lat=lat,lng=lng}\\ Just (lat,lng) <- map latLng points]
                 ,strokeWidth = 2
                 ,strokeColor = color
                 ,fillColor = Nothing
                 } \\ {ContactMapRegion|regionId,color,points} <- regions]
toLeafletLayer {ContactMapLayer|def=CMMarkersLayer markers}
    = ObjectLayer (flatten [conv m \\ m=:{ContactMapMarker|position} <- markers | hasLatLng position])
where
    conv m=:{ContactMapMarker|track=Just _}     = [trackline m,marker m]
    conv m                                      = [marker m]

    marker {ContactMapMarker|markerId,title,position,heading,type,selected,track}
        = Marker {LeafletMarker|markerId = markerId, title = title, position = pos position, icon = fmap (\t -> iconIndex heading t selected) type, selected = selected}

    trackline {ContactMapMarker|markerId,position,track=Just (ContactTrack positions)}
        = Polyline {polylineId=markerId +++ "-TRACK"
                   ,points = [pos position : [{LeafletLatLng|lat=lat,lng=lng} \\ (_,lat,lng) <- positions]]
                   ,strokeWidth = 1
                   ,strokeColor = "#000"
                   }

    pos (PositionLatLng (lat,lng)) = {LeafletLatLng|lat=lat,lng=lng}
    pos (PositionDescription _ (Just(lat,lng))) = {LeafletLatLng|lat=lat,lng=lng}

    iconIndex heading type selected = (cat type + ( (maybe 24 (\d -> d / 15) heading) + (if selected 25 0)) * 5)

toLeafletPerspective :: ContactMapPerspective -> LeafletPerspective
toLeafletPerspective {ContactMapPerspective|center,zoom,cursor,bounds}
    = {LeafletPerspective|center=toLeafletLatLng center,zoom=zoom,cursor=fmap toLeafletLatLng cursor,bounds=fmap toLeafletBounds bounds}

toLeafletLatLng :: !(!Real,!Real) -> LeafletLatLng
toLeafletLatLng (lat,lng) = {LeafletLatLng|lat=lat,lng=lng}

toLeafletBounds :: !(!(!Real,!Real),!(!Real,!Real)) -> LeafletBounds
toLeafletBounds (sw,ne) = {LeafletBounds|southWest=toLeafletLatLng sw,northEast=toLeafletLatLng ne}

fromLeafletMap :: ContactMap LeafletMap -> ContactMap
fromLeafletMap contactMap leafletMap
    = {ContactMap|contactMap
      &perspective = fromLeafletPerspective leafletMap.LeafletMap.perspective
      ,layers = [fromLeafletLayer cl ll \\ cl <- contactMap.ContactMap.layers & ll <- leafletMap.LeafletMap.layers]
      }

fromLeafletPerspective :: LeafletPerspective -> ContactMapPerspective
fromLeafletPerspective {LeafletPerspective|center,cursor,zoom,bounds}
    = {ContactMapPerspective|center=fromLeafletLatLng center,zoom=zoom,cursor=fmap fromLeafletLatLng cursor,bounds=fmap fromLeafletBounds bounds}

fromLeafletLayer :: ContactMapLayer LeafletLayer -> ContactMapLayer
fromLeafletLayer cl=:{ContactMapLayer|def=CMMarkersLayer markers} (ObjectLayer objects)
    = {ContactMapLayer|cl & def = CMMarkersLayer (map updateMarker markers)}
where
    updateMarker cm=:{ContactMapMarker|markerId} = maybe cm (updateMarker` cm) ('DM'.get markerId indexedLeafletMarkers)
    updateMarker` cm lm=:{LeafletMarker|position,selected}
        = {ContactMapMarker|cm & position=PositionLatLng (fromLeafletLatLng position), selected = selected}
    indexedLeafletMarkers = 'DM'.fromList [(markerId,m) \\ Marker m=:{LeafletMarker|markerId} <- objects]

fromLeafletLayer cl ll = cl

selectionFromLeafletMap :: LeafletMap -> [String]
selectionFromLeafletMap {LeafletMap|layers} = flatten (map selection layers)
where
    selection (ObjectLayer markers) = [markerId \\ Marker {LeafletMarker|markerId,selected} <- markers | selected]
    selection _ = []

fromLeafletLatLng :: !LeafletLatLng -> (!Real,!Real)
fromLeafletLatLng {LeafletLatLng|lat,lng} = (lat,lng)

fromLeafletBounds :: !LeafletBounds -> (!(!Real,!Real),!(!Real,!Real))
fromLeafletBounds {LeafletBounds|southWest,northEast} = (fromLeafletLatLng southWest,fromLeafletLatLng northEast)

standardPerspective :: Shared ContactMapPerspective
standardPerspective = sharedStore "standardPerspective" defaultValue

standardMapLayers :: Shared [ContactMapLayer]
standardMapLayers = sharedStore "standardMapLayers" [{ContactMapLayer|title="Local OSM tiles",def=CMTileLayer "/tiles/{z}/{x}/{y}.png"}]

