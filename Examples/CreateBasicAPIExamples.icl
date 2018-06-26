module CreateBasicAPIExamples

import StdFile, StdBool, StdMisc, StdFunc, StdList, StdString, StdTuple
import Control.Monad => qualified join
import Control.Applicative
import Data.Functor
import Data.Func
import Data.Error
import Data.Maybe
import Data.Tuple
import System.FilePath
import System.File
import System.Directory
import System.OSError
import Text

:: IOT t a = IOT .(*World -> *(t a, *World))

runIOT (IOT i) = i

instance Functor (IOT t) | Functor t
where
	fmap f m = IOT (appFst (fmap f) o runIOT m)
instance Applicative (IOT t) | Applicative t
where
	pure a = IOT (tuple (pure a))
	(<*>) mf ma = IOT \w->
		let (f, w`) = runIOT mf w in appFst ((<*>) f) (runIOT ma w`)
instance Monad (IOT (MaybeError e))
where
	bind ma a2mb = IOT \w->case runIOT ma w of
		(Error e, w) = (Error e, w)
		(Ok a, w) = runIOT (a2mb a) w
//seqErrorsSt :: !(.st -> (MaybeError e a,!.st)) (a .st -> u:(!MaybeError e b, !.st)) !.st -> v:(MaybeError e b, !.st), [u <= v]	

filterDirs :: FilePath -> Bool
filterDirs "." = False
filterDirs ".." = False
filterDirs _ = True

recurse :: FilePath -> IOT (MaybeError OSError) [FilePath]
recurse root
	| endsWith ".dcl" root = pure [root]
	= IOT (getFileInfo root) >>= \fi->if fi.directory
		(IOT (readDirectory root)
			>>= fmap flatten o mapM (recurse o (</>) root) o filter filterDirs)
		(pure [])

makeExs :: [FilePath] -> [String]
makeExs i = 
	[ "basicAPIExamples :: [Workflow]\n"
	, "basicAPIExamples =\n"
	, "\t[",join "\n\t," (map makeEx i), "\n\t]\n"]
makeEx i = concat ["workflow \"", i, "\" '", i, "'.doc '", i, "'.workflow"]

Start w
# (io, w) = stdio w
# (mcwd, w) = runIOT (recurse "BasicAPIExamples") w
| isError mcwd = abort ("Error in getting the files: " +++ toString (snd (fromError mcwd)) +++ "\n")
# io = foldl (<<<) io (makeExs (fromOk mcwd))
# (ok, w) = fclose io w
| not ok = abort "Couldn't close stdio\n"
= (w, mcwd)
