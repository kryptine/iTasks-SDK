module TestToolBar
import iTasks

test :: Task String
test =   viewInformation () [] "This is the content of the container"
     <<@ ApplyLayout (wrapUI UIToolBar) 

Start world = startEngine test world
