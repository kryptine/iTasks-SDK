implementation module SystemTasks

from TSt import :: Task, :: TSt
from TSt import mkInstantTask
from TSt import accWorldTSt
from TSt import qualified getCurrentUser

from UserDB import :: User, :: UserId
from UserDB	import qualified getUser

from iTasks import class iTask
import GenPrint, GenParse, GenVisualize, GenUpdate

getCurrentUser :: Task User
getCurrentUser = mkInstantTask "getCurrentUserId" getCurrentUser`
where
	getCurrentUser` tst
		# (cur,tst)	= TSt@getCurrentUser tst
		= UserDB@getUser cur tst

getDefaultValue :: Task a | iTask a
getDefaultValue = mkInstantTask "getDefaultValue" getDefaultValue`
where
	getDefaultValue` tst
		= accWorldTSt defaultValue tst