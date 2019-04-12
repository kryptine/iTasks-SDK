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
	, "import iTasks\n"
	, "import Text.HTML\n"
	, "import qualified iTasks.Extensions.Admin.UserAdmin\n"
	, "\n"
	, join "\n" ["import qualified " +++ toDots i\\i<-i]
	, "\n\n"
	, "Start :: *World -> *World\n"
	, "Start world = doTasks {WorkflowCollection|name=name,welcomeMessage=Just message,workflows=basicAPIExamples} world\n"
	, "where\n"
	, "\tname = \"iTasks Example Collection\"\n"
	, "\tmessage = DivTag []\n"
	, "\t\t[Text \"If you want to try the examples in a multi user setting, you can log in with one of the demo users\"\n"
	, "\t\t,BrTag []\n"
	, "\t\t,Text \"For example:\"\n"
	, "\t\t\t,UlTag []\n"
	, "\t\t\t\t[LiTag [] [Text \"Administrator (username: root, password: root)\"]\n"
	, "\t\t\t\t,LiTag [] [Text \"Alice (username: alice, password: alice)\"]\n"
	, "\t\t\t\t,LiTag [] [Text \"Bob (username: bob, password: bob)\"]\n"
	, "\t\t\t\t,LiTag [] [Text \"Carol (username: carol, password: carol)\"]\n"
	, "\t\t\t\t]\n"
	, "\t\t\t]\n"
	, "\n"
	, "basicAPIExamples :: [Workflow]\n"
	, "basicAPIExamples =\n"
	, "\t[",join "\n\t," (defaultWfs ++ exampleWfs), "\n\t]\n"]
where
	defaultWfs = ["restrictedTransientWorkflow \"Users\" \"User management\" [\"admin\"] 'iTasks.Extensions.Admin.UserAdmin'.manageUsers"]
	exampleWfs = map (\i->concat ["'", toDots i, "'.wf \"", toString (insertSpaces 0 (dropExtension i)), "\""]) i
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
