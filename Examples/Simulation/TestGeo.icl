module TestGeo

import MovingEntity, GeoRoutines, iTasks

Start = test6 //(toDegrees p3,test4)


test0 = map toDegrees [p1,p2,p3,p4,p5]
test1 = pe (moveToTarget entity p2 100)
test2 = pe (updatePosition entity  1)
test3 = map pe (steps 101 entity)
test4 = map pdd (stepmove 201 entity p3)
test6 = map pde (routeWaypoints entity2 waypoints)

test5 = (radials2degrees (getDirectionToPosition p1 p2), "p1p2\n"
         ,radials2degrees (getDirectionToPosition p1 p3), "p1p3\n"
         ,radials2degrees (getDirectionToPosition p1 p4), "p1p4\n"
         ,radials2degrees (getDirectionToPosition p3 p2), "p3p2\n"
         ,radials2degrees (getDirectionToPosition p3 p1), "p3p1\n"
         ,radials2degrees (getDirectionToPosition p3 p4), "p3p4\n"
         ,radials2degrees (getDirectionToPosition p2 p4), "p2p4\n"
         ,radials2degrees (getDirectionToPosition p4 p2), "p4p2\n"
         ,radials2degrees (getDirectionToPosition p2 p1), "p2p1\n"
         ,radials2degrees (getDirectionToPosition p4 p1), "p4p1\n")


//moveAlongWayPoints entity waypoints 1
degrees2radials deg = deg * pi / 180.0
radials2degrees rad = rad * 180.0 / pi
pi = 3.141592653589

steps n me = sts me 0 
where sts me t | t == n    = []
               | otherwise # ne = updatePosition me  t
                           = [ne : sts ne (t+1)]
                       
stepmove n me  target = sts me 0 
where sts me t | t == n    = []
               | otherwise # ne = moveToTarget me target t
                           = [ne : sts ne (t+1)]
                       
routeWaypoints me wps = steps me wps 0
where steps me wps t | abs me.speed < 0.1 = []
                     | otherwise          # (ne,nwps)= moveAlongWayPoints me wps t
                                          = [me : steps ne nwps (t+1)]
                     
pe {position,angVelocity} = toDegrees position
pd {position,angVelocity,direction} = radials2degrees direction
pde {position,angVelocity,direction} = (radials2degrees direction, toDegrees position,"\n")
pdd e=:{position,angVelocity} = (radials2degrees targetdir, " ",distance position p3, " ",radials2degrees angVelocity,"\n")
where targetdir  = getDirectionToPosition position p3

entity    = {ne & direction = degrees2radials -90.0, speed = 300.0}
where ne = newMovingEntity 0 (fromDegrees (45.0,10.0)) 300.0 0

entity2    = ne
where ne = newMovingEntity 0 (fromDegrees (45.0,10.0)) 300.0 0

waypoints = [p2,p3,p4,p1]

p1 = fromDegrees(45.0,10.0)
p2 = translateDeg p1 0.0 10000.0
p3 = translateDeg p2 90.0 10000.0
p4 = translateDeg p3 -180.0 10000.0
p5 = translateDeg p4 -90.0 10000.0
