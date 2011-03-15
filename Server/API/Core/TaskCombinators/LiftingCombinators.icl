implementation module LiftingCombinators

from Error import ::MaybeError(..), isError, fromError, fromOk
import TSt

from OSError import ::MaybeOSError, ::OSError, ::OSErrorCode, ::OSErrorMessage
from ExceptionCombinators import :: OSException(..), instance toString OSException

appWorld :: !(*World -> *World) -> Task Void
appWorld fun = mkInstantTask ("Run world function", "Run a world function.") (\tst -> (TaskFinished Void,appWorldTSt fun tst))

accWorld :: !(*World -> *(!a,!*World)) -> Task a | iTask a
accWorld fun = mkInstantTask ("Run world function", "Run a world function and get result.") (mkTaskFunction (accWorldTSt fun))

accWorldError :: !(*World -> (!MaybeError e a, !*World)) !(e -> err) -> Task a | iTask a & TC, toString err
accWorldError fun errf = mkInstantTask ("Run a world function", "Run a world function with error handling.") f
where
	f tst
	# (res, tst) 	= accWorldTSt fun tst
	| isError res	= (taskException (errf (fromError res)), tst)
	= (TaskFinished (fromOk res), tst)

accWorldOSError :: !(*World -> (!MaybeOSError a, !*World)) -> Task a | iTask a
accWorldOSError fun = accWorldError fun OSException
