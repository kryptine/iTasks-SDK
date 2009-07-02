 implementation module iDataState

import StdEnv, StdMaybe, ArgEnv, Directory
import iDataTrivial, EncodeDecode
import GenPrint, GenParse
import dynamic_string
import FormId
import StdMaybe
import Map, Text

derive gPrint 	(,), (,,), (,,,), Maybe, Void
derive gParse	(,), (,,), (,,,), Maybe, Void
derive bimap	(,), (,,), (,,,), Maybe, Void

// This module controls the handling of state forms and the communication with the browser
// iData states are maintained in a binary tree

// The are currently two storage formats for iData values: 
// 1. a string format, generically generated; works for any first order type.
// A generic parser is needed to convert the string back to a value of the required type.
// 2. a static (= no dynamic linker needed) dynamic; works for any type, including higher order types.
// But watch it: only the application which wrotes the dynamic, can read it back: no plug ins! recompiling means: all information lost!
// A distinction is made between old states (states retrieved from the html form)
// and new states (states of newly created forms and updated forms)

:: *FormStates 	=												// collection of states of all forms
				{ updates	:: ![FormUpdate]					// indicates what has changed: which form, which postion, which value
				, cache		:: !(Map String FormState)			// TODO: use this instead of ad-hoc tree
				}
				
:: FormState	= { format	:: !Format							// Encoding method used for serialization
				  , life	:: !Lifespan						// Its life span
				  , new		:: !Bool							// Is it a newly created state
				  }

:: Format		= PlainStr 	!.String 							// Either a string is used for serialization
				| StatDyn	!Dynamic 							// Or a dynamic which enables serialization of functions defined in the application (no plug ins yet)
				| CLDBStr   .String (*DataFile -> *DataFile)	// In case a new value has to be stored in a Cleans database file

// DataFile OPTION
readDataFile id datafile 
:== IF_DataFile (loadDataFile id datafile)
					(abort "Reading from DataFile, yet option is swiched off\n", 
					 abort "Reading from DataFile, yet option is swiched off\n")

writeDataFile id val 
:== IF_DataFile (storeDataFile id val)
					(\_ -> abort "Writing to DataFile, yet option is swiched off\n")

deleteDataFile id datafile
:== IF_DataFile (removeDataFile id datafile)
					(abort "Deleting data from DataFile, yet option is swiched off\n")

mkFormStates :: ![HtmlState] ![FormUpdate] ->  *FormStates
mkFormStates states updates
	= { updates		= updates
	  , cache		= fromList	[(formid, {format = toVal format state, life = lifespan, new = False})
	  							\\ {formid,lifespan,state,format} <- states
	  							]
	  }
where
	toVal PlainString   string	= PlainStr string									// string that has to be parsed in the context where the type is known
	toVal StaticDynamic string	= StatDyn (string_to_dynamic {s \\ s <-: string})	// recover the dynamic


getFormUpdates :: !String !*FormStates -> (![FormUpdate], !*FormStates)
getFormUpdates id formstates=:{updates} = ([update \\ update =:{FormUpdate|formid} <- updates | id == formid],formstates)

getAllUpdates :: !*FormStates -> (![FormUpdate], !*FormStates)
getAllUpdates formstates =:{updates} = (updates, formstates)

getUpdatedIds :: !*FormStates -> (![String],!*FormStates)
getUpdatedIds formstates=:{updates} = (removeDup [formid \\ {FormUpdate|formid} <- updates] ,formstates)

getState :: !(FormId a) !*FormStates !*NWorld -> (!Bool,!Maybe a,!*FormStates,!*NWorld)	| iPrint, iParse, iSpecialStore a
getState formid=:{id} formstates=:{FormStates|cache} nworld
	//Check if the value is available in the cache tree first
	# (mbState, cache) = getU id cache
	= case mbState of
		Just state = (not state.new, unpackState state, {FormStates| formstates & cache = cache}, nworld)
		//Search the store on disk
		Nothing					
			# (mbState, nworld) = searchInStore formid nworld
			= case mbState of
				Just (a, state)
					//Add read value to cache tree
					# cache		= put id state cache
					= (True, Just a, {FormStates| formstates & cache = cache}, nworld)
				_
					= (False, Nothing, {FormStates| formstates & cache = cache}, nworld)
where
	unpackState {FormState | format = PlainStr s}			= parseString s
	unpackState {FormState | format = CLDBStr s _}			= parseString s
	unpackState {FormState | format = StatDyn (v :: a^)}	= Just v
	unpackState _											= Nothing
	
	//Look for plainstring on disk (Drup)
	searchInStore {id, lifespan = LSDataFile, storage = PlainString} nworld=:{datafile}
		# (value,datafile)	= readDataFile id datafile
		= case value of
			Nothing		= (Nothing, {nworld & datafile = datafile})
			Just a		= (Just (a, {format = PlainStr (printToString a), life = LSDataFile, new = False}), {nworld & datafile = datafile})
	
	//Look for staticdynamic on disk (Drup)
	searchInStore {id, lifespan = LSDataFile, storage = StaticDynamic} nworld=:{datafile}
		# (value,datafile)	= readDataFile id datafile
		= case value of 
			Nothing		= (Nothing, {nworld & datafile = datafile})
			Just s		= case string_to_dynamic` s of
							dyn=:(a :: a^)	= (Just (a, {format = StatDyn dyn, life = LSDataFile, new = False}), {nworld & datafile = datafile})
							_				= (Nothing, {nworld & datafile = datafile})
	
	//Look for plainstring on disk (Plain text)
	searchInStore {id, lifespan = LSTxtFile, storage = PlainString} nworld
		# (string,nworld)	= IF_Client ("",nworld) (readStateFile id nworld)
		= case parseString string of
			Just a			= (Just (a, {format = PlainStr string, life = LSTxtFile, new = False}),nworld)
			Nothing			= (Nothing, nworld)
	searchInStore {id, lifespan = LSTxtFileRO, storage = PlainString} nworld
		# (string,nworld)	= IF_Client ("",nworld) (readStateFile id nworld)
		= case parseString string of
			Just a			= (Just (a, {format = PlainStr string, life = LSTxtFile, new = False}),nworld)
			Nothing			= (Nothing, nworld)

	//Look for staticdynamic on disk (Plain text)
	searchInStore {id, lifespan = LSTxtFile, storage = StaticDynamic} nworld
		# (string,nworld)	= IF_Client ("",nworld) (readStateFile id nworld)
		= case string of 
			""	= (Nothing,nworld)
			_	= case string_to_dynamic` string of
				dyn=:(a :: a^)	= (Just (a, {format = StatDyn dyn, life = LSTxtFile, new = False}),nworld)
				_				= (Nothing, nworld)
	searchInStore {id, lifespan = LSTxtFileRO, storage = StaticDynamic} nworld
		# (string,nworld)	= IF_Client ("",nworld) (readStateFile id nworld)
		= case string of 
			""	= (Nothing,nworld)
			_	= case string_to_dynamic` string of
				dyn=:(a :: a^)	= (Just (a, {format = StatDyn dyn, life = LSTxtFile, new = False}),nworld)
				_				= (Nothing, nworld)
	
	//Other formid types have no store
	searchInStore _ nworld	= (Nothing, nworld)

setState :: !(FormId a) a !*FormStates !*NWorld -> (!*FormStates, !*NWorld) | iPrint, iSpecialStore a
setState formid=:{id,lifespan} a formstates=:{cache} nworld
	= case lifespan of
		LSTxtFileRO
			//Special case for read only lifespan, only add a read-only state once
			# (mbState,cache) = getU id cache
			= case mbState of
				Nothing = ({formstates & cache = put id (packState formid a) cache} ,nworld)
				Just _	= ({formstates & cache = cache}, nworld)
		_
			= ({formstates & cache = put id (packState formid a) cache}, nworld)		
where
	packState {id,storage=PlainString,lifespan=LSDataFile} a	= {format = CLDBStr (printToString a) (writeDataFile id a), life = lifespan, new = True}
	packState {id,storage=StaticDynamic,lifespan} a 			= {format = StatDyn (dynamic a), life = lifespan, new = True}
	packState {id,storage,lifespan} a 							= {format = PlainStr (printToString a), life = lifespan, new = True}

deleteStates :: !String !*FormStates !*NWorld -> (!*FormStates,!*NWorld)
deleteStates prefix formstates=:{cache} nworld
	# cache		= fromList [(id,state) \\ (id,state) <- toList cache | not (startsWith prefix id)]
	# nworld 	= deleteStateFiles prefix nworld
	= ({formstates & cache = cache}, nworld)
	//TODO: delete states in datafile

copyStates :: !String !String !*FormStates !*NWorld -> (!*FormStates,!*NWorld)
copyStates frompf topf formstates=:{cache} nworld
	# cache = fromList (flatten [[(fid,state): if (startsWith frompf fid) [(newId fid, {state & new = True})] [] ]\\ (fid, state) <- toList cache])
	# nworld = copyStateFiles frompf topf nworld
	= ({formstates & cache = cache}, nworld)
	//TODO: copy states in datafile
where
	newId fid	= topf +++ (fid % (size frompf, size fid))
	

getHtmlStates :: !String !*FormStates -> (![HtmlState], !*FormStates)
getHtmlStates prefix formstates=:{cache}
	# states = [mkHtmlState fid state \\ (fid, state) <- toList cache | startsWith prefix fid && storeOnClient state] 
	= (states,formstates)
where
	storeOnClient {life} = isMember life [LSPage,LSSession,LSClient]
	
	mkHtmlState fid {life,format=PlainStr s}	= {formid=fid,lifespan=life,state=s,format=PlainString}
	mkHtmlState fid {life,format=StatDyn d}		= {formid=fid,lifespan=life,state=dynamic_to_string d,format=StaticDynamic}

flushCache :: !*FormStates !*NWorld -> (!*FormStates, !*NWorld)
flushCache formstates=:{cache} nworld
	= (formstates, writeStates (toList cache) nworld)
where
	writeStates [] nworld = nworld
	writeStates [s:ss] nworld = writeStates ss (writeState s nworld) 
	
	writeState (sid,{format,life = LSDataFile, new = True}) nworld=:{datafile}
		= case format of
			CLDBStr   string dfilefun	= {nworld & datafile = dfilefun datafile}										// last value is stored in curried write function
			StatDyn dynval				= {nworld & datafile = writeDataFile sid (dynamic_to_string dynval) datafile}	// write the dynamic as a string to the datafile

	writeState (sid,{format,life  = LSTxtFile, new = True}) nworld
		= IF_Client nworld 
		 ( case format of
				PlainStr string			= writeStateFile sid string nworld
				StatDyn  dynval			= writeStateFile sid (dynamic_to_string dynval) nworld)

	writeState _ nworld					= nworld

// writing and reading of persistent states to a file
writeStateFile :: !String !String !*NWorld -> *NWorld 
writeStateFile filename serializedstate env
	# ((ok,mydir),env) = pd_StringToPath iDataStorageDir env
	| not ok = abort ("writeState: cannot create path to " +++ iDataStorageDir)
	#(_,env)		= case getFileInfo mydir env of
							((DoesntExist,fileinfo),env)	= createDirectory mydir env
							(_,env)							= (NoDirError,env)
	# (ok,file,env)	= fopen (iDataStorageDir +++ "/" +++ filename +++ ".txt") FWriteData env
	| not ok	 	= env
	# file			= fwrites serializedstate file  // DEBUG
	# (ok,env)		= fclose file env
	= env

readStateFile :: !String !*NWorld -> (!String,!*NWorld) 
readStateFile filename env
	# ((ok,mydir),env) = pd_StringToPath iDataStorageDir env
	| not ok = abort ("readState: cannot create path to " +++ iDataStorageDir)
	#(_,env)		= case getFileInfo mydir env of
							((DoesntExist,fileinfo),env)	= createDirectory mydir env
							(_,env)							= (NoDirError,env)
	# (ok,file,env)	= fopen (iDataStorageDir +++ "/" +++ filename +++ ".txt") FReadData env
	| not ok 		= ("",env)
	# (string,file)	= freadfile file
	| not ok 		= ("",env)
	# (ok,env)		= fclose file env
	= (string,env)
	where
		freadfile :: *File -> (String, *File)
		freadfile file = rec file ""
		  where rec :: *File String -> (String, *File)
		        rec file acc # (string, file) = freads file 100
		                     | string == "" = (acc, file)
		                     | otherwise    = rec file (acc +++ string)

deleteStateFile :: !String !*NWorld -> *NWorld
deleteStateFile  filename env
	# directory								= iDataStorageDir
	# ((ok,path),env) 						= pd_StringToPath (directory +++ "\\" +++ filename +++ ".txt") env
	| not ok								= abort "Cannot delete indicated iData"
	# (_,env)								= fremove path env
	= env 

deleteStateFiles :: !String !*NWorld -> *NWorld
deleteStateFiles prefix env
	# ((ok,mydir),env) = pd_StringToPath iDataStorageDir env
	| not ok = abort ("deleteStates: cannot create path to " +++ iDataStorageDir)
 	# ((err,files),env) = getDirectoryContents mydir env
 	| err <> NoDirError	= abort ("deleteStates: cannot read directory " +++ iDataStorageDir)
 	= removeFiles prefix mydir files env
where
	removeFiles prefix mydir [] env = env
	removeFiles prefix mydir [f:fs] env
		| startsWith prefix f.fileName
			# (err,env) = fremove (pathDown mydir f.fileName) env 
			= removeFiles prefix mydir fs env
		| otherwise
			= removeFiles prefix mydir fs env
			
copyStateFiles :: !String !String !*NWorld -> *NWorld
copyStateFiles fromprefix toprefix env
	# ((ok,mydir),env) = pd_StringToPath iDataStorageDir env
	| not ok = abort ("deleteStates: cannot create path to " +++ iDataStorageDir)
 	# ((err,files),env) = getDirectoryContents mydir env
 	| err <> NoDirError	= abort ("deleteStates: cannot read directory " +++ iDataStorageDir)
  	= copyFiles fromprefix toprefix files env
where	
	copyFiles fromprefix toprefix [] env = env
	copyFiles fromprefix toprefix [f:fs] env
		| startsWith fromprefix f.fileName
			# sfile	= (iDataStorageDir +++ "\\" +++ f.fileName)
			# dfile = (iDataStorageDir +++ "\\" +++ toprefix +++ (f.fileName % (size fromprefix, size f.fileName)))
			# env	= copyFile sfile dfile env
			= copyFiles fromprefix toprefix fs env
		| otherwise
			= copyFiles fromprefix toprefix fs env
	  
traceStates :: !*FormStates -> (!HtmlTag,!*FormStates)
traceStates formstates=:{cache}
	= (TableTag [ClassAttr "debug-table"] [header : rows], formstates)
where
	rows = map nodeTrace (toList cache)
	header = TrTag [] [ThTag [] [Text "Form ID"],ThTag [] [Text "Inspected"],ThTag [] [Text "Lifespan"],ThTag [] [Text "Format"], ThTag [] [Text "Value"]]
	
	nodeTrace (id,{format,life,new}) = TrTag [] [TdTag [] [Text id], TdTag [] [Text (if new "Yes" "No")], TdTag [] [Text (toString life)]: toCells format]
	
	toCells (PlainStr str) 		= [TdTag [] [Text "String"],TdTag [] [Text str]]
	toCells (StatDyn  dyn) 		= [TdTag [] [Text "S_Dynamic"],TdTag [] [Text "---"]]
	toCells (CLDBStr  str _) 	= [TdTag [] [Text "DataFile"],TdTag [] [Text str]]

traceUpdates :: !*FormStates -> (!HtmlTag,!*FormStates)
traceUpdates formstates=:{updates}
	= (TableTag [ClassAttr "debug-table"] [header : rows], formstates)
where
	header	= TrTag [] [ThTag [] [Text "Form ID"], ThTag [] [Text "Input ID"], ThTag [] [Text "Value"]]
	rows	= [TrTag [] [TdTag [] [Text formid], TdTag [] [Text (toString inputid)], TdTag [] [Text value]] \\ {FormUpdate|formid,inputid,value} <- updates ]

// utility
string_to_dynamic` :: !String -> Dynamic
string_to_dynamic` s = string_to_dynamic {s` \\ s` <-: s}

pathDown :: !Path !String -> Path
pathDown (RelativePath steps) step = RelativePath (steps ++ [PathDown step]) 
pathDown (AbsolutePath dn steps) step = AbsolutePath dn (steps ++ [PathDown step])

copyFile :: !String !String !*env -> *env | FileSystem env
copyFile sfilename dfilename env
	# (ok,sfile,env) = fopen sfilename FReadData env
	| not ok = abort ("copyFile: Could not open " +++ sfilename +++ " for reading")
	# (ok,dfile,env) = fopen dfilename FWriteData env
	| not ok = abort ("copyFile: Could not open " +++ dfilename +++ " for writing")
	# (sfile,dfile) = copy sfile dfile
	# (ok,env) = fclose sfile env
	| not ok = abort ("copyFile: Could not close " +++ sfilename)
	# (ok,env) = fclose dfile env
	| not ok = abort ("copyFile: Could not close " +++ dfilename)
	= env
where
	copy sfile dfile
		# (ok,c,sfile) = freadc sfile
		| ok
			# dfile = fwritec c dfile
			= copy sfile dfile
		| otherwise
			# (err,sfile)= ferror sfile
			| err		= abort "copyFile: read error during copy"
			| otherwise = (sfile,dfile)	
	
// debugging code 
print_graph :: !a -> Bool;
print_graph a = code {
.d 1 0
jsr _print_graph
.o 0 0
pushB TRUE
}

my_dynamic_to_string :: !Dynamic -> {#Char};
my_dynamic_to_string d
| not (print_graph d)
= abort ""
#! s=dynamic_to_string d;
| not (print_graph (tohexstring s))
= abort "" 
# d2 = string_to_dynamic {c \\ c <-: s};
| not (print_graph d2)
= abort ""
= s;

tohexstring :: !{#Char} -> {#Char};
tohexstring s = {tohexchar s i \\ i<-[0..2*size s-1]};

tohexchar :: !{#Char} !Int -> Char;
tohexchar s i
# c=((toInt s.[i>>1]) >> ((1-(i bitand 1))<<2)) bitand 15;
| c<10
= toChar (48+c);
= toChar (55+c);