implementation module iTasks.Internal.Util

import StdBool, StdChar, StdList, StdFile, StdMisc, StdArray, StdString, StdTuple, StdFunc, StdGeneric, StdOrdList
import Data.Maybe, Data.Tuple, Data.Func, System.Time, System.OS, Text, System.FilePath, System.Directory, Text.GenJSON, Data.Error, Data.GenEq
import Data.Error, System.OSError, System.File
import iTasks.Internal.IWorld
import iTasks.WF.Definition
import iTasks.Internal.TaskEval
import iTasks.UI.Definition
from iTasks.Internal.IWorld 		import :: IWorld{current}, :: TaskEvalState
from iTasks.Extensions.DateTime import :: Date{..}, :: Time{..}, :: DateTime(..)
import qualified Control.Monad as M
import qualified Data.Map as DM
import qualified Data.Set as DS
from Data.Map import :: Map

show :: ![String] !*World -> *World
show lines world
	# (console,world)	= stdio world
	# console			= seqSt (\s c -> fwrites (s +++ "\n") c) lines console
	# (_,world)			= fclose console world
	= world

iShow :: ![String] !*IWorld -> *IWorld
iShow lines iworld = {iworld & world = show lines iworld.world}

tmToDateTime :: !Tm -> DateTime
tmToDateTime tm
	= {DateTime| day = tm.Tm.mday, mon = 1 + tm.Tm.mon, year = 1900 + tm.Tm.year
	  ,hour = tm.Tm.hour, min = tm.Tm.min, sec= tm.Tm.sec}

toCanonicalPath	:: !FilePath !*World -> (!FilePath,!*World)
toCanonicalPath path world
	| isAbsolute path = (canonicalize path,world)
	| otherwise
		= case getCurrentDirectory world of
			(Ok curDir,world)	= (canonicalize (curDir</>path), world)
			(_,world)		= (canonicalize path,world)
where
	isAbsolute path = IF_POSIX_OR_WINDOWS (startsWith {pathSeparator} path) (size path >= 2 && isUpper path.[0] && path.[1] == ':')

	canonicalize path = join {pathSeparator} (undot [] (split {pathSeparator} path))

	undot acc []				= reverse acc
	undot []  ["..":ds]			= undot [] ds
	undot [_:acc] ["..":ds]			= undot acc ds
	undot acc [".":ds]			= undot acc ds
	undot [] ["":ds]			= undot [""] ds //Only allowed at the beginning
	undot acc ["":ds]			= undot acc ds
	undot acc [d:ds] 			= undot [d:acc] ds

(>-=) infixl 1 :: (*env -> *(MaybeError e a, *env)) (a -> *(*env -> (MaybeError e b, *env))) *env -> (MaybeError e b, *env)
(>-=) a b w
	# (mca, w) = a w
	= case mca of
		Error e = (Error e, w)
		Ok a = (b a) w

liftIWorld :: .(*World -> *(.a, *World)) *IWorld -> *(.a, *IWorld)
liftIWorld f iworld
# (a, world) = f iworld.world
= (a, {iworld & world=world})

apIWTransformer :: *env (*env -> *(MaybeError TaskException (TaskResult a), *env)) -> *(TaskResult a, *env)
apIWTransformer iw f = case f iw of
	(Error e, iw) = (ExceptionResult e, iw)
	(Ok tv, iw) = (tv, iw)

generateRandomString :: !Int !*IWorld -> (!String, !*IWorld)
generateRandomString length iworld=:{IWorld|random}
	= (toString (take length [toChar (97 +  abs (i rem 26)) \\ i <- random]) , {IWorld|iworld & random = drop length random})

isRefreshForTask :: !Event !TaskId -> Bool
isRefreshForTask (RefreshEvent taskIds _) taskId = 'DS'.member taskId taskIds
isRefreshForTask ResetEvent _ = True
isRefreshForTask _ _ = False

mkTaskEvalInfo :: !TaskTime -> TaskEvalInfo
mkTaskEvalInfo ts = {TaskEvalInfo|lastEvent=ts,removedTasks=[]}

mkUIIfReset :: !Event !UI -> UIChange
mkUIIfReset ResetEvent ui = ReplaceUI ui
mkUIIfReset _ ui = NoChange

murmurHash :: !String -> Int
murmurHash s
	# (h1,h2) = runblocks 0 (nblocks-1) seed seed
	# (h1,h2) = runtail (len bitand 15) h1 h2
	# h1 = h1 bitxor len
	# h2 = h2 bitxor len
	# h1 = h1 + h2
	# h2 = h2 + h1
	# h1 = fmix64 h1
	# h2 = fmix64 h2
	# h1 = h1 + h2
	# h2 = h2 + h1
	= h1 bitxor h2 // NB: real murmur returns both h1 and h2 here
where
	seed = 0 // TODO
	len = size s
	nblocks = len >> 4
	c1 = 0x87c37b91114253d5
	c2 = 0x4cf5ad432745937f

	runblocks :: !Int !Int !Int !Int -> (!Int, !Int)
	runblocks i end h1 h2
		| i >= end = (h1,h2)
		# k1 = get_int_from_string (i<<4) s
		# k2 = get_int_from_string ((i<<4)+8) s
		# k1 = (rotl64 (k1 * c1) 31) * c2
		# h1 = ((rotl64 (h1 bitxor k1) 27) + h2) * 5 + 0x52dce729
		# k2 = (rotl64 (k2 * c2) 33) * c1
		# h2 = ((rotl64 (h2 bitxor k2) 31) + h1) * 5 + 0x38495ab5
		= runblocks (i+1) end h1 h2

	runtail :: !Int !Int !Int -> (!Int, !Int)
	// NB: because the input is from copy_to_string its length is always a multiple of 8!
	runtail taillen h1 h2
		| taillen == 0
			= (h1,h2)
		| taillen <= 8
			# k1 = get_int_from_string (len bitand -8) s
			# k1 = if (taillen<=4)
				(if (taillen<=2)
					(if (taillen==1) (k1 bitand 0xff)     (k1 bitand 0xffff))
					(if (taillen==3) (k1 bitand 0xffffff) (k1 bitand 0xffffffff)))
				(if (taillen <=6)
					(if (taillen==5) (k1 bitand 0xffffffffff)     (k1 bitand 0xffffffffffff))
					(if (taillen==7) (k1 bitand 0xffffffffffffff) k1))
			# k1 = (rotl64 (k1 * c1) 31) * c2
			# h1 = h1 bitxor k1
			= (h1,h2)
		| otherwise // taillen <= 15
			# k2 = get_int_from_string ((len bitand -8)-8) s
			# k2 = if (taillen<=12)
				(if (taillen<=10)
					(if (taillen== 9) (k2 bitand 0xff)     (k2 bitand 0xffff))
					(if (taillen==11) (k2 bitand 0xffffff) (k2 bitand 0xffffffff)))
				(if (taillen <=14)
					(if (taillen==13) (k2 bitand 0xffffffffff)     (k2 bitand 0xffffffffffff))
					(if (taillen==15) (k2 bitand 0xffffffffffffff) (abort "murmurHash: runtail\n")))
			# k2 = (rotl64 (k2 * c2) 33) * c1
			# h2 = h2 bitxor k2
			= runtail (taillen-8) h1 h2

	fmix64 :: !Int -> Int
	fmix64 k
		# k = k bitxor (k >> 33)
		# k = k * 0xff51afd7ed558ccd
		# k = k bitxor (k >> 33)
		# k = k * 0xc4ceb9fe1a85ec53
		# k = k bitxor (k >> 33)
		= k

	get_int_from_string :: !Int !String -> Int
	get_int_from_string offset s = code {
		push_a_b 0
		pop_a 1
		addI
		load_i 16
	}

rotl64 x r :== (x << r) bitor (x >> (64-r))
