module TestDashEditlet
import iTasks
import iTasks.Extensions.Dashboard

test = viewInformation "LED" [] LightOnRed

Start world = startEngine test world
