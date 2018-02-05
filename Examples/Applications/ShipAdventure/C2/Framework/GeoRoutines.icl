implementation module C2.Framework.GeoRoutines

import StdEnv
import Math.Geometry
import Data.Maybe

earthRadius =: 6371000.0

// 3d part

// distance in meters between two positions
distance :: LatLng LatLng -> Real
distance p1 p2
  # p1xyz = normalizedlatLong2XYZ p1
  # p2xyz = normalizedlatLong2XYZ p2
  # ip1p2 = inner3 p1xyz p2xyz
  # alpha = acos ip1p2
  = alpha * earthRadius

// return direction from first position to second position
getDirectionToPosition :: LatLng LatLng -> Angle
getDirectionToPosition (lat1, lon1) (lat2, lon2)
  # lat1 = toRad lat1
  # lon1 = toRad lon1
  # lat2 = toRad lat2
  # lon2 = toRad lon2
  # x    = sin (lon2 - lon1) * cos lat2
  # y    = cos lat1 * sin lat2 - sin lat1 * cos lat2  * cos (lon2 - lon1)
  = normalize (rad (atan (x / y) + correction x y))
  where
  correction x y
    | y < 0.0 && x < 0.0  = ~pi
    | y < 0.0 && x > 0.0  = pi
    | y < 0.0 && x == 0.0 = ~pi
    | otherwise           = 0.0

normalizedlatLong2XYZ :: LatLng -> (!Real, !Real, !Real)
normalizedlatLong2XYZ (lat, lng)
  # lat = toRad lat
  # lng = toRad lng
  # r   = cos lat
  = (r * cos lng, r * sin lng, sin lat)

inner3 :: (!Real, !Real, !Real) (!Real, !Real, !Real) -> Real
inner3 (a1, a2, a3) (b1, b2, b3) = a1 * b1 + a2 * b2 + a3 * b3

inner2 :: (!Real, !Real) (!Real, !Real) -> Real
inner2 (a1, a2) (b1, b2) = a1 * b1 + a2 * b2

normalizeLatLong :: LatLng -> LatLng
normalizeLatLong (lat, lon)
  # lat        = toRad lat
  # lon        = toRad lon
  # (lat, lon) = nn lat lon
  = (rad lat, rad (normlon lon))
  where
  nn lat lon
    | lat > pi / 2.0  = (pi - lat,  lon + pi)
    | lat < ~pi / 2.0 = (~pi - lat, lon + pi)
    | otherwise       = (lat, lon)
  normlon lon
    | lon > pi  = normlon (lon - 2.0 * pi)
    | lon < ~pi = normlon (lon + 2.0 * pi)
    | otherwise = lon

translate :: LatLng !Angle !Real -> LatLng
translate (lat, lon) direction distance
  # lat       = toRad lat
  # lon       = toRad lon
  # direction = toRad direction
  # newlat    = lat + cos direction * distance / earthRadius
  # newlon    = lon + sin direction * distance / (earthRadius * cos lat)
  = normalizeLatLong (rad newlat, rad newlon)

translateAlongCurve :: LatLng !Angle !Angle !Real -> LatLng
translateAlongCurve pos direction angle radius
  # angle          = toRad angle
  # direction      = toRad direction
  # angleDirection = angle / 2.0
  # distlinear     = 2.0 * radius * sin angleDirection
  = translate pos (rad (direction + angleDirection)) distlinear

turningPoint :: LatLng LatLng LatLng !Real -> LatLng
turningPoint p1 p2 p3 radius
  # heading12 = getDirectionToPosition p1 p2
  # heading23 = getDirectionToPosition p2 p3
  # deltaH    = heading23 - heading12
  # distTurn  = distanceForTurning deltaH radius
  # distpoint = translate p1 heading12 (distance p1 p2 - distTurn)
  = distpoint

// 2d functions
distanceForTurning :: !Angle !Real -> Real
distanceForTurning deltaAngle radius
  # deltaAngle = toRad deltaAngle
  = radius * (1.0 - cos deltaAngle) / sin deltaAngle

acceleration2radius :: !Real !Real -> Real
acceleration2radius accel speed = speed * speed / accel

angularVelocity2radius :: !Real !Real -> Real
angularVelocity2radius w speed= speed / w

radius2angularVelocity :: !Real !Real -> Real
radius2angularVelocity radius speed = speed / radius

acceleration2angularVelocity :: !Real !Real -> Real
acceleration2angularVelocity accel speed = accel / speed

angularVelocity2acceleration :: !Real !Real -> Real
angularVelocity2acceleration w speed = speed / w

radius2acceleration :: !Real !Real -> Real
radius2acceleration radius speed = speed * speed / radius

dirDist2vector :: !Angle !Real -> (!Real, !Real)
dirDist2vector direction distance
  # direction = toRad direction
  = (distance * sin direction, distance * cos direction)

dif2 :: (!Real, !Real) (!Real, !Real) -> (!Real, !Real)
dif2 (x1, x2) (y1, y2) = (x1 - y1, x2 - y2)

normsq :: !(!Real, !Real) -> Real
normsq p = inner2 p p

// Relative position in plane
relPosition :: LatLng LatLng -> (!Real, !Real)
relPosition p1 p2 = dirDist2vector (getDirectionToPosition p1 p2) (distance p1 p2)

deltaDirection :: !Angle !Angle -> Angle
deltaDirection currentDir nextDir = rad delta
  where
  deltaTemp = toRad nextDir - toRad currentDir
  delta | deltaTemp < ~pi = 2.0 * pi + deltaTemp
        | deltaTemp > pi  = -2.0 * pi + deltaTemp
        | otherwise       = deltaTemp

interceptionDirection :: !LatLng !Angle !Real !LatLng !Angle !Real -> Maybe Angle
interceptionDirection pos1 direction1 speed1 pos2 direction2 speed2
  # a = relPosition pos1 pos2
  # v = dirDist2vector direction2 speed2
  = case interceptionTime a  v speed1 of
      Just t
        # newpos = translate pos2 direction2 (t * speed2)
        = Just (headingToPosition pos1 newpos)
      _ = Nothing

headingToPosition :: !LatLng !LatLng -> Angle
headingToPosition (lon1, lat1) (lon2, lat2)
  # lon1 = toRad lon1
  # lat1 = toRad lat1
  # lon2 = toRad lon2
  # lat2 = toRad lat2
  = rad (atan2 (sin(lon2 - lon1) * cos(lat2)) (cos(lat1) * sin(lat2) - sin(lat1) * cos(lat2) * cos(lon2 - lon1)))

interceptionTime :: !Point !Point !Real -> Maybe Real
interceptionTime (ax, ay) (vx, vy) s
  # h1                 = vx * vx + vy * vy - s * s
  # h1                 = if (h1 == 0.0) 0.001 h1
  # minusPHalf         = 0.0 - (ax * vx + ay * vy) / h1
  # discriminant       = minusPHalf * minusPHalf - (ax * ax + ay * ay) / h1
  | discriminant < 0.0 = Nothing
  # root               = sqrt discriminant
  # t1                 = minusPHalf + root
  # t2                 = minusPHalf - root
  # tMin               = min t1 t2
  # tMax               = max t1 t2
  # t                  = if (tMin > 0.0) tMin tMax
  | t < 0.0            = Nothing
  = Just t

cpaTime :: !LatLng !Angle !Speed !LatLng !Angle !Speed -> Real
cpaTime pos1 direction1 speed1 pos2 direction2 speed2
  # u                  = dirDist2vector direction1 speed1
  # v                  = dirDist2vector direction2 speed2
  # relspeed=:(rx, ry) = dif2 u v
  | rx == 0.0 && ry == 0.0 = 0.0
  # relpos             = relPosition pos1 pos2
  = inner2 relpos relspeed / normsq relspeed

cpaDistance :: !LatLng !Angle !Speed !LatLng !Angle !Speed -> Real
cpaDistance pos1 direction1 speed1 pos2 direction2 speed2
  # cpatime = cpaTime pos1 direction1 speed1 pos2 direction2 speed2
  # n1      = translate pos1 direction1 (speed1 * cpatime)
  # n2      = translate pos2 direction2 (speed2 * cpatime)
  = distance n1 n2

atan2 :: !Real !Real -> Real
atan2 0.0 0.0 = abort "atan2 0 0"
atan2 y 0.0
  | y > 0.0 = pi / 2.0
  | y < 0.0 = 0.0 - (pi / 2.0)
atan2 y x
  | x > 0.0             = atan (y / x)
  | x < 0.0 && y >= 0.0 = atan (y / x) + pi
  | x < 0.0 && y < 0.0  = atan (y / x) - pi
