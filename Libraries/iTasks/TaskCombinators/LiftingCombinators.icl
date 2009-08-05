implementation module LiftingCombinators

import TSt

appWorld :: !String !(*World -> *(!a,!*World)) -> (Task a) | iTask a
appWorld label fun = mkBasicTask label (accWorldTSt fun)