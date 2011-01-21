implementation module LiftingCombinators

import TSt

appWorld :: !(*World -> *World) -> Task Void
appWorld fun = mkInstantTask ("Run world function", "Run a world function.") (\tst -> (TaskFinished Void,appWorldTSt fun tst))

accWorld :: !(*World -> *(!a,!*World)) -> Task a | iTask a
accWorld fun = mkInstantTask ("Run world function", "Run a world function and get result.") (mkTaskFunction (accWorldTSt fun))
