definition module Task.Extensions

import iTasks

editStore :: String (Shared [a]) -> Task () | iTask a & Eq a & Ord a

addToStore :: [a] !(Shared [a]) -> Task () | iTask a

appendTitledTopLevelTask :: String (Task a) -> Task TaskId | iTask a

startTopLevelOnce :: (Task a) Action String (Task b) -> Task () | iTask a & iTask b

maybeCancel :: String (Task a) -> Task (Maybe a) | iTask a

deadline :: Date (Task a) -> Task (Maybe a) | iTask a

deadlineWith :: Date a (Task a) -> Task a | iTask a
