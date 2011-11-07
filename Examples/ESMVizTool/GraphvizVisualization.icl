implementation module GraphvizVisualization

import GenVisualize, GenUpdate, GenPrint, GenParse
//import TSt, Types, Util, HttpUtil
import Util, HttpUtil
import StdFile, StdTuple, StdList, StdBool, StdArray, StdString

//from Directory import pd_StringToPath, getFileInfo, createDirectory, instance == DirError
from Directory import createDirectory
//from Directory import :: Path(..), :: PathStep, :: DirError(..), :: FileInfo, :: DiskName
from Directory import :: FilePath(..), :: MaybeOSError, :: OSError, :: OSErrorCode, :: OSErrorMessage

import Graphviz, launch

derive bimap (,), Maybe
derive class iTask	NodeState, Digraph, 
					Arrow, ArrowShape, ArrowType, ClusterMode, Color, CompassPoint, DirType, DotPoint, 
					EdgeAttribute, EdgeStyle, GraphAttribute, LayerId, LayerList, LayerRange, Margin, 
					NodeAttribute, NodeDef, NodeShape, NodeStyle, OutputMode, Pad, PageDir, Pointf, 
					RankDir, RankType, Ratio, Rect, SelectedItem, Side, Sizef, StartStyle, StartType, ViewPort
					
config_file_name		:== "Graphviz.config"
commentsymbol			:== '%'
dot_exe_path_name		:== "DOT_PATH"
target file				= "Static\\" + file
toGIF file				= "-Tgif -o " + "\"" + gifext file + "\" \"" + dotext file + "\""
toMAP file name			= "-Tcmapx" + " -Glabel=" + name + " -o " + "\"" + mapext file + "\" \"" + dotext file + "\""
gifext file				= file + ".gif"
mapext file				= file + ".map"
dotext file				= file + ".dot"
					
//Special task for manipulating digraphs
updateDigraph :: !Digraph -> Task String
updateDigraph digraph = mkInteractiveTask "editDigraph" "Digraph editor" taskfun
where
	taskfun tst=:{taskNr}
		# (events,tst)		= getEvents tst
		# node = (http_getValue "node" events "")
		| node <> ""
			= (TaskFinished node, tst)
		# (value,tst)		= accWorldTSt (obtainValueFromConfig dot_exe_path_name) tst
		| isNothing value	= error ("Could not obtain " + dot_exe_path_name + " from " + config_file_name + ".") tst
		# exe				= fromJust value
		# tst				= appWorldTSt (ensureDirectory (target "")) tst
		# (ok,tst)			= accWorldTSt (writefile` (target (dotext name)) (printDigraph (enhanceDigraphWithLinks digraph))) tst
		| not ok			= error ("Could not write Digraph to " + target (dotext name) + ".") tst
		# ((ok,exit),tst)	= accWorldTSt (collect3 (launch exe (toGIF (target name)))) tst
		| not ok			= error ("Creation of " + gifext (target name) + " failed. Exit code = " + toString exit + ".") tst
		# ((ok,exit),tst)	= accWorldTSt (collect3 (launch exe (toMAP (target name) name))) tst
		| not ok			= error ("Creation of " + mapext (target name) + " failed. Exit code = " + toString exit + ".") tst
		# ((ok,lines),tst)	= accWorldTSt (collect3 (readfile` (mapext (target name)))) tst
		| not ok			= error ("Reading of " + mapext (target name) + " failed.") tst	
		# lines				= map enhanceMAPlineWithOnClickEvent lines
		# html				= "<img src=\"" + (gifext name) + "\" usemap=\"#" + name + "\" />" + join "" lines + toString legend
		# taskpanel 		= TUIHtmlContainer
								{ TUIHtmlContainer
								| id = "taskform-" + taskid + "-graph"
								, html = html							
								}
		# tst = setTUIDef ([taskpanel],[]) [] tst
		= (TaskBusy, tst)
	where
		name	= iTaskId taskNr "Graphviz"
		taskid	= taskNrToString taskNr
		
		error msg tst
			# tst = setTUIDef ([TUIHtmlContainer
									{ TUIHtmlContainer
									| id = "taskform-" + taskid + "-graph"
									, html = msg
									}],[]) [] tst
			= (TaskBusy, tst)

		enhanceMAPlineWithOnClickEvent :: !String -> String
		enhanceMAPlineWithOnClickEvent line
			| line%(0,5) == "<area "
				| size line <= 6 || isNothing href_bounds 
							= line
				| otherwise	= line%(0,fst href-1) + 
							  "onclick=\"fireTaskEvent('" + taskid + "','node','" + titletext + "');\" " + 
							  line%(snd href+1,size line-1)
			| line%(0,4) == "<map "
							= "<map id=\"" + name + "\" name=\"" + name + "\">\n"
			| otherwise
							= line
		where
			href_bounds		= boundsOfKeyValue "href="  line
			title_bounds	= boundsOfKeyValue "title=" line
			href			= fromJust href_bounds
			title			= fromJust title_bounds
			titletext		= line%(fst title+7,snd title-1)
			
		legend  =	DivTag []
						[ H3Tag [] [Text "Legend:"]
						, Text "Double circled state: intial state.", BrTag []
						, Text "Red state: current state (change state by click).", BrTag []
						, Text "Blue state: all defined inputs are shown.", BrTag []
						, Text "Blue transitions: on current trace.", BrTag []
						, Text "Red transitions: an issue was found on this transition."
						]
			
boundsOfKeyValue :: !String !String -> Maybe (!Int,!Int)
boundsOfKeyValue key str
	= case [i \\ i<-[0..size str-size key] | str%(i,i+size key-1) == key] of
		[i : _]			= case [j \\ j<-[i..size str-1] | str.[j]=='\"'] of
							[_,close:_] = Just (i,close)
							otherwise	= Nothing
		otherwise		= Nothing
join :: !String ![String] -> String
join sep [] = ""
join sep [x:[]] = x
join sep [x:xs] = x + sep + (join sep xs)

obtainValueFromConfig :: !String !*env -> (!Maybe String,!*env) | FileSystem env
obtainValueFromConfig name env
	# (ok,file,env)		= fopen config_file_name FReadText env
	| not ok			= (Nothing,env)
	# (value,file)		= obtainValueFromFile name file
	# (ok,env)			= fclose file env
	| not ok			= (Nothing,env)
	| otherwise			= (value,  env)
where
	obtainValueFromFile :: !String !*File -> (!Maybe String,!*File)
	obtainValueFromFile name file
		# (lines,file)	= readlines file
		# value			= case [ skipSpace (line%(name_length,size line-2)) \\ line<-lines
						                                                     | line.[0] <> commentsymbol 
						                                                    && size line > name_length
						                                                    && line%(0,name_length-1) == name
						       ] of [v:_]	= Just v
						            _		= Nothing
		= (value,file)
	where
		name_length		= size name

enhanceDigraphWithLinks :: !Digraph -> Digraph
enhanceDigraphWithLinks (Digraph name graphAtts nodeDefs selected)
	= Digraph name graphAtts 
		[  NodeDef nr st [ NAtt_URL "#" /*("#")*/ : nodeAtts ] edges 
		\\ NodeDef nr st nodeAtts edges <- nodeDefs
		] selected

ensureDirectory :: !String !*env -> *env | FileSystem env	// PA++
ensureDirectory pathname env
	# ((ok,path), env)	= pd_StringToPath pathname env
	| not ok			= env
	# ((err,info),env)	= getFileInfo path env
	| err<>NoDirError	= snd (createDirectory path env)
	| otherwise			= env

writefile` :: !String ![String] !*env -> (!Bool,!*env) | FileSystem env
writefile` fileName content env
	# (ok,file,env)		= fopen fileName FWriteText env
	| not ok			= (False,env)
	# file				= foldl (<<<) file content
	= fclose file env

readfile`  :: !String !*env -> (!Bool,![String],!*env) | FileSystem env
readfile` fileName env
	# (ok,file,env)		= fopen fileName FReadText env
	| not ok			= (False,[],env)
	# (content,file)	= readlines file
	# (ok,env)			= fclose file env
	= (ok,content,env)

readlines :: !*File -> (![String],!*File)
readlines file
	# (end,file)		= fend file
	| end				= ([],file)
	# (line, file)		= freadline file
	# (lines,file)		= readlines file
	= ([line:lines],file)

collect3 :: (.s -> (.a,.b,.s)) .s -> (.(.a,.b),.s)
collect3 f st
	# (a,b,st)			= f st
	= ((a,b),st)

skipSpace :: !String -> String
skipSpace str			= toString (dropWhile isSpace (fromString str))

instance + String where (+) s t = s + t