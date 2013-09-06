implementation module iTasks.Framework.Tonic

import qualified Data.SharedDataSource as DSD
import iTasks.Framework.Shared
import iTasks.Framework.IWorld
import iTasks.API.Core.CoreCombinators
import iTasks.API.Core.SystemTypes
import iTasks.API.Core.SystemData

tonicShare :: !User -> Shared String
tonicShare currentUser = sharedStore ("tonicTraceFor" +++ username currentUser) ""
  where username (AnonymousUser sid)         = sid
        username (AuthenticatedUser uid _ _) = uid

import StdDebug

instance tune Tonic where
  tune (Tonic traceMsg) (Task eval) = Task eval`
  where eval` event repOpts state iworld=:{IWorld|currentUser}
          # (_, iworld) = trace_n ("setting trace in Tonic share: " +++ traceMsg) ('DSD'.write traceMsg (tonicShare currentUser) iworld)
          = eval event repOpts state iworld

