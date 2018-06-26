module CreateBasicAPIExamples

import StdEnv
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

(>>=) infixl 1
(>>=) :== seqErrorsSt

filterDirs :: FilePath -> Bool
filterDirs "." = False
filterDirs ".." = False
filterDirs _ = True

seq [] = tuple (Ok [])
seq [e:es] = e >>= \a->seq es >>= \as->tuple (Ok [a:as])

recurse :: FilePath -> .(*World -> *(MaybeError OSError [FilePath], !*World))
recurse root
	| endsWith ".dcl" root = tuple (Ok [root])
	= getFileInfo root >>= \fi->if fi.directory
			(readDirectory root >>= ((o) (appFst (fmap flatten))) o seq o map (recurse o (</>)root) o filter filterDirs)
			(tuple (Ok []))

makeExs :: [FilePath] -> [String]
makeExs i = 
	[ "module BasicAPIExamples\n"
	, "\n"
	, "import iTasks"
	, "\n"
	, join "\n" ["import qualified " +++ toDots i\\i<-i]
	, "\n\n"
	, "Start :: *World -> *World\n"
	, "Start world = startEngine\n"
	, "\t[ publish \"/\" (\\_->loginAndManageWorkList title basicAPIExamples <<@ ApplyLayout (setUIAttributes (titleAttr title)))\n"
	, "\t] world\n"
	, "where\n"
	, "\ttitle = \"iTasks Example Collection\"\n"
	, "\n"
	, "basicAPIExamples :: [Workflow]\n"
	, "basicAPIExamples =\n"
	, "\t[",join "\n\t," (map (\i->concat ["'", toDots i, "'.wf \"", toString (insertSpaces 0 (dropExtension i)), "\""]) i), "\n\t]\n"]
where
	toDots = join "." o split (toString pathSeparator) o dropExtension
	insertSpaces i s
		| i == size s = []
		| s % (i, i+2) == "API" = [' ','A','P','I':insertSpaces (i+3) s]
		| isUpper s.[i] && (i == 0  || s.[i-1] <> '/') = [' ',s.[i]:insertSpaces (i+1) s]
		= [s.[i]:insertSpaces (i+1) s]

Start w
# (io, w) = stdio w
# (mcwd, w) = recurse "BasicAPIExamples" w
| isError mcwd = abort ("Error in getting the files: " +++ toString (snd (fromError mcwd)) +++ "\n")
# io = foldl (<<<) io (makeExs (fromOk mcwd))
# (ok, w) = fclose io w
| not ok = abort "Couldn't close stdio\n"
= w
