implementation module iTasks.API.Extensions.Admin.TonicAdmin

import iTasks
import StdMisc, Data.Tuple, Text, Data.Either, Data.Functor
import iTasks._Framework.SDS, iTasks._Framework.Generic.Interaction, iTasks.API.Core.Types, iTasks._Framework.Tonic
from StdFunc import seq
import qualified Data.Map as DM

tonic :: Task ()

tonic = tonicDashboard []

tonicDashboard :: [TaskAppRenderer] -> Task ()
tonicDashboard rs = ((tonicStaticBrowser rs <<@ FullScreen <<@ Title "Static Blueprints")
               -||- (tonicDynamicBrowser rs <<@ FullScreen <<@ Title "Dynamic Blueprints")) <<@ ArrangeWithTabs <<@ FullScreen
