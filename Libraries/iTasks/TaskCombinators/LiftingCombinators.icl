implementation module LiftingCombinators

import TSt

appWorld :: !(*World -> *World) -> Task Void
appWorld fun = mkInstantTask "appWorld" (\tst -> (Void,appWorldTSt fun tst))

accWorld :: !(*World -> *(!a,!*World)) -> Task a | iTask a
accWorld fun = mkInstantTask "accWorld" (accWorldTSt fun)
