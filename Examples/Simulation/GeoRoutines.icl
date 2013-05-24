implementation module GeoRoutines

import StdEnv

//Start = getHeadingToPositionDeg (0.0,3.2) (0.1,3.1) 

:: LatLng :== (!Real,!Real)

earth_radius :: Real
earth_radius = 6371000.0

pi = 3.141592653589

degrees2radials deg = deg * pi / 180.0
radials2degrees rad = rad * 180.0 / pi

fromDegrees :: (Real,Real) -> LatLng
fromDegrees (e,t) = (degrees2radials e,degrees2radials t)

toDegrees ::  LatLng ->(Real,Real)
toDegrees (e,t) = (radials2degrees e,radials2degrees t)

// 3d part
distance :: LatLng LatLng -> Real
distance p1 p2 = alpha * earth_radius
where p1xyz = normalizedlatLong2XYZ p1
      p2xyz = normalizedlatLong2XYZ p2
      ip1p2 = inner3 p1xyz p2xyz
      alpha = acos ip1p2

getHeadingToPositionDeg :: LatLng LatLng -> Real
getHeadingToPositionDeg (lat1,lon1) (lat2,lon2) 
= radials2degrees (getHeadingToPosition (degrees2radials lat1,degrees2radials lon1) (degrees2radials lat2,degrees2radials lon2))

getHeadingToPosition :: LatLng LatLng -> Real
getHeadingToPosition (lat1,lon1) (lat2,lon2) 
=  atan ((sin(lon2 - lon1) * cos lat2) / 
   (cos lat1 * sin lat2- sin lat1 * cos lat2  * cos(lon2 - lon1)))
            
normalizedlatLong2XYZ :: LatLng -> (!Real,!Real,!Real)
normalizedlatLong2XYZ (lat,lng) = (r * cos lng,r * sin lng, sin lat)
where r = cos lat

inner3 :: (!Real,!Real,!Real) (!Real,!Real,!Real) -> Real
inner3 (a1,a2,a3) (b1,b2,b3) = a1*b1+a2*b2+a3*b3

inner2 :: (!Real,!Real) (!Real,!Real) -> Real
inner2 (a1,a2) (b1,b2) = a1*b1+a2*b2

normalizeLatLong :: LatLng -> LatLng
normalizeLatLong (lat,lon) = (fst nn, normlon (snd nn))
where nn | lat > pi / 2.0       = (pi - lat,      lon + pi)
         | lat < 0.0 - pi / 2.0 = (0.0 - pi - lat,lon + pi)
         | otherwise            = (lat,lon)
      normlon lon | lon > pi       = normlon (lon - 2.0 * pi)
                  | lon < 0.0 - pi = normlon (lon + 2.0 * pi)
                  | otherwise      = lon
      
translate :: LatLng !Real !Real -> LatLng
translate (lat,lon) direction distance = normalizeLatLong (newlat,newlon)
where newlat = lat + cos direction * distance / earth_radius
      newlon = lon + sin direction * distance / (earth_radius * cos lat)
      
translateCurve :: LatLng !Real !Real !Real -> LatLng
translateCurve pos direction angle radius = translate pos (direction + angleDirection) distlinear
where angleDirection = angle / 2.0
      distlinear     = 2.0 * radius * sin angleDirection

// 2d functions
dist4Corner :: !Real !Real -> Real
dist4Corner deltaAngle radius = radius * (1.0 - cos deltaAngle) / sin deltaAngle

acceleration2radius :: !Real !Real -> Real
acceleration2radius accel speed = speed * speed / accel

angularVelocity2radius :: !Real !Real -> Real
angularVelocity2radius w speed= speed / w

radius2angularVelocity :: !Real !Real -> Real
radius2angularVelocity radius speed= speed / radius

acceleration2angularVelocity :: !Real !Real -> Real
acceleration2angularVelocity accel  speed= accel / speed;

angularVelocity2acceleration :: !Real !Real -> Real
angularVelocity2acceleration w speed = speed / w

radius2acceleration :: !Real !Real -> Real
radius2acceleration  radius  speed = speed * speed / radius

dirDist2vector :: !Real !Real -> (!Real,!Real)
dirDist2vector direction  distance = (distance * sin direction, distance *  cos direction)

dif2 :: (!Real,!Real) (!Real,!Real) -> (!Real,!Real)
dif2 (x1,x2) (y1,y2) = (x1-y1,x2-y2)

normsq :: (!Real,!Real) -> Real
normsq p = inner2 p p

relPosition :: LatLng LatLng -> (!Real,!Real)
relPosition p1 p2 = dirDist2vector (getHeadingToPosition p1 p2) (distance p1 p2)











