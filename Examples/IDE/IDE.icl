module IDE

import iTasks
import iTasks.API.Extensions.Development.Codebase
import iTasks.API.Extensions.Development.CleanCode

test :: Task ()
test
    =   navigateCodebase []
    >&> \cs -> whileUnchanged cs
        \c -> case c of
            Nothing = viewInformation "Select a module first..." [] ()
            Just (d,m)  = viewCleanModule d m


Start :: *World -> *World
Start world = startEngine test world
