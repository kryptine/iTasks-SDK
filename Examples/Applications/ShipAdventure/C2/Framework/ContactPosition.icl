implementation module C2.Framework.ContactPosition
import iTasks
import iTasks.Extensions.GIS.Leaflet
import qualified Data.Map as DM
import Data.Functor, Text
import qualified Text.Parsers.ZParsers.ParsersKernel as PK
import qualified Text.Parsers.ZParsers.ParsersDerived as PD
import qualified Control.Applicative as CA
from Control.Applicative import class Alternative, class Applicative

from Text.Parsers.ZParsers.ParsersKernel import :: Parser, instance Alternative (Parser p t), instance pure (Parser s t), instance <*> (Parser s t), instance Functor (Parser s t)
import C2.Framework.GeoRoutines
import Math.Geometry
import Data.Maybe
import iTasks.Extensions.Platform
import iTasks.UI.Definition, iTasks.UI.Editor, iTasks.UI.Editor.Controls, iTasks.UI.Editor.Modifiers

derive JSONEncode ContactPosition
derive JSONDecode ContactPosition

gEditor{|ContactPosition|} = bijectEditorValue printPosition parsePosition textField
gText{|ContactPosition|} _ val = [maybe "" printPosition val]

derive gDefault ContactPosition
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
    parseLat    = (parseFloat 'PD'. <&&> northSouth) 'PD'. <@ (\(lat,f) -> f lat)
    northSouth  = ('PK'.symbol 'N' 'PD'. <@ const id) 'PK'. <!> ('PK'.symbol 'S' 'PD'. <@ const (~)) 'PK'. <!> ('PK'.yield id)
    parseLng    = (parseFloat 'PD'. <&&> eastWest) 'PD'. <@ (\(lng,f) -> f lng)
    eastWest    = ('PK'.symbol 'E' 'PD'. <@ const id) 'PK'. <!> ('PK'.symbol 'W' 'PD'. <@ const (~)) 'PK'. <!> ('PK'.yield id)
    whitespace  = 'PD'. <!+> ('PK'.satisfy isSpace)
    separator   = 'PK'.token [','] 'PK'. <!> whitespace

    parseFloat  = (nums 'PD'. <&&> frac) 'PD'. <@ (\(n,f) -> toReal (toString (n++f)))
    frac        = ('PK'.symbol '.' 'PD'. <:&> nums) 'PK'. <!> 'PK'.yield []
    nums        = 'PD'. <!+> ('PK'.satisfy isDigit)

leafletMapContactPosition :: LatLng -> LeafletLatLng
leafletMapContactPosition (lat, lng) = {LeafletLatLng|lat=toDeg lat,lng= toDeg lng}

derive class iTask ContactMap, ContactMapMarker, ContactMapMarkerType

derive JSONEncode ContactMapPerspective
derive JSONDecode ContactMapPerspective
derive gEditor ContactMapPerspective
derive gText ContactMapPerspective
derive gEq ContactMapPerspective

gDefault{|ContactMapPerspective|}
    =  {ContactMapPerspective|center = (deg 52.948300, deg 4.776007), zoom = 7, cursor = Nothing} //(Full coast centered on Den Helder)

defaultPerspective :: ContactMapPerspective
defaultPerspective = defaultValue

/*contactToMapMarker :: Bool Bool Contact -> ContactMapMarker
contactToMapMarker ais selected contact=:{Contact|contactNo,name,position,heading}
    = {ContactMapMarker
      |markerId = (if ais "a" "c") +++ toString contactNo
      ,title    = name
      ,position = fromJust position
      ,heading  = fmap (\(Degrees h) -> h) heading
      ,type     = Just (type ais contact)
      ,selected = selected
      }
where
    type True _ = CMAIS
    type _ {Contact|needsHelp=True}                 = CMNeedsHelp
    type _ {Contact|group=Just "Kustwacht varend"}  = CMUnit
    type _ {Contact|group=Just "KNRM"}              = CMUnit
    type _ _                                        = CMOther
*/


cat CMAIS       = 0
cat CMUnit      = 0
cat CMOther     = 0
cat CMNeedsHelp = 1
cat CMHostile   = 4
cat CMSuspect   = 2
cat CMSelf      = 0
cat CMHVU       = 1
cat _           = 0

toLeafletMap :: ContactMap -> LeafletMap
toLeafletMap {ContactMap|perspective,markers}
    = {LeafletMap|perspective = toLeafletPerspective perspective
      ,icons = [icon i \\ i <- [1..250]]
	  ,tilesUrls = [TILESERVER]
      ,objects = convMarkers markers //Just the baselayer
      }
where
    convMarkers markers = [conv m \\ m=:{ContactMapMarker|position} <- markers]
    conv {ContactMapMarker|markerId,title,position,heading,type}
        = Marker {LeafletMarker|markerId = LeafletObjectID markerId, title = title, position = toLeafletLatLng position, icon = fmap (\t -> LeafletIconID (iconIndex heading t False)) type,popup = Nothing}

	icon i = {LeafletIcon|iconId=LeafletIconID (toString i),iconUrl ="/ship-icons/"+++toString i+++".png",iconSize=(24,24)}
    iconIndex heading type selected = toString (cat type + ( (maybe 24 (\d -> toInt d / 15) heading) + (if selected 25 0)) * 5)

toLeafletPerspective :: ContactMapPerspective -> LeafletPerspective
toLeafletPerspective {ContactMapPerspective|center,zoom,cursor}
    = {LeafletPerspective|center=toLeafletLatLng center,zoom=zoom,bounds=Nothing}

toLeafletLatLng :: !LatLng -> LeafletLatLng
toLeafletLatLng (lat,lng) = {LeafletLatLng | lat = toDeg lat, lng = toDeg lng}

fromLeafletLatLng :: !LeafletLatLng -> LatLng
fromLeafletLatLng {LeafletLatLng | lat, lng} = (deg lat, deg lng)

fromLeafletMap :: LeafletMap -> ContactMap
fromLeafletMap {LeafletMap|perspective,objects}
    = {ContactMap|perspective = fromLeafletPerspective perspective
       ,markers=toMarkers objects}
where
    toMarkers objects
        = [{ContactMapMarker|markerId=markerId,title=Nothing,position = fromLeafletLatLng position, type=Nothing,heading=Nothing,selected=False}
          \\ Marker {LeafletMarker|markerId = LeafletObjectID markerId,position} <- objects]

fromLeafletPerspective :: LeafletPerspective -> ContactMapPerspective
fromLeafletPerspective {LeafletPerspective|center,zoom}
    = {ContactMapPerspective|center=fromLeafletLatLng center,zoom=zoom,cursor=Nothing}


