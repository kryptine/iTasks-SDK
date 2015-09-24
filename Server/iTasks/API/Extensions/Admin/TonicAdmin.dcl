definition module iTasks.API.Extensions.Admin.TonicAdmin

import iTasks
import iTasks._Framework.Tonic.Images

tonicDashboard :: [TaskAppRenderer] -> Task ()

tonic :: Task ()

tonicStaticBrowser    :: [TaskAppRenderer] -> Task ()

tonicStaticWorkflow   :: [TaskAppRenderer] -> Workflow

tonicDynamicBrowser   :: [TaskAppRenderer] -> Task ()

tonicDynamicWorkflow  :: [TaskAppRenderer] -> Workflow

