implementation module SystemTasks

from TSt import :: Task, :: TSt
from TSt import mkInstantTask
from TSt import qualified getCurrentUser

from UserDB import :: User, :: UserId
from UserDB	import qualified getUser

getCurrentUser :: Task User
getCurrentUser = mkInstantTask "getCurrentUserId" getCurrentUser`
where
	getCurrentUser` tst
		# (cur,tst)	= TSt@getCurrentUser tst
		= UserDB@getUser cur tst