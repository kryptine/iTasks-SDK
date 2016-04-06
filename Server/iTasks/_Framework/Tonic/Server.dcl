definition module iTasks._Framework.Tonic.Server

import iTasks
from iTasks._Framework.Tonic.AbsSyn import :: ModuleName
from iTasks._Framework.Tonic.Types import :: TonicMessage

standAloneTonicViewer :: Task ()

acceptAndViewTonicTraces :: Task (Bool, [(TonicMessage, Bool)])
