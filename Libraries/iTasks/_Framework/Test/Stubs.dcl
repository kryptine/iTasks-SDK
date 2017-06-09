definition module iTasks._Framework.Test.Stubs

from iTasks._Framework.IWorld import :: IWorld
from iTasks.UI.Editor import :: VSt

//TEST STUBS
toStubIWorld :: *World -> *IWorld

fromStubIWorld :: *IWorld -> *World

toStubVSt :: *IWorld -> *VSt

fromStubVSt :: *VSt -> *IWorld
