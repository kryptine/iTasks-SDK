definition module Tests.Unit.FrameworkStubs

from iTasks._Framework.IWorld import :: IWorld
from iTasks.UI.Editor import :: VSt, :: USt

//TEST STUBS
toStubIWorld :: *World -> *IWorld

fromStubIWorld :: *IWorld -> *World

toStubVSt :: *IWorld -> *VSt

fromStubVSt :: *VSt -> *IWorld

toStubUSt :: *IWorld -> *USt

fromStubUSt :: *USt -> *IWorld
