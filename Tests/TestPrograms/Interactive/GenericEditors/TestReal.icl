module TestReal

import iTasks, iTasks.Internal.Test.Definition 

test :: Task Real
test = testCommonInteractions "Real"

Start world = startEngine test world
