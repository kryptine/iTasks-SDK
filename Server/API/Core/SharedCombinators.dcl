definition module SharedCombinators

import Void, Maybe
from IWorld				import :: IWorld
from SharedDataSource	import mapReadWrite, mapRead, mapWrite
from SharedDataSource	import qualified :: RWShared

:: RWShared r w	:== 'SharedDataSource'.RWShared r w IWorld
:: Shared a		:== RWShared a a
:: ROShared r	:== RWShared r Void
:: WOShared w	:== RWShared Void w