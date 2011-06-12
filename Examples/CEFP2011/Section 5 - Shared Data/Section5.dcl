definition module Section5

// Examples showing the usage of shared data

import iTasks

flows5 :: [Workflow]

:: Tweet  :== (User,String)
joinTweets  :: String (Shared [Tweet]) -> Task Void

