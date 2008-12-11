 implementation module iDataState

import StdArray, StdList, StdOrdList, StdString, StdTuple, StdFile, ArgEnv, StdMaybe, Directory
import iDataTrivial, EncodeDecode
import GenPrint, GenParse
import dynamic_string
import EstherBackend
import FormId
import StdMaybe
import StdDebug

derive gPrint 	(,), (,,), (,,,), Maybe, Void
derive gParse	(,), (,,), (,,,), Maybe, Void
derive gerda 	(,), (,,), (,,,), Void
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
				{ fstates 	:: !*FStates						// internal tree of states
				, updates	:: ![FormUpdate]					// indicates what has changed: which form, which postion, which value
				, instates	:: ![HtmlState]						// states received from the browser (only stored for tracing)
				}		

:: FStates		:== Tree_ (!String,!FormState)					// each form needs a different string id

:: Tree_ a 		= Node_ !(Tree_ a) !a !(Tree_ a) | Leaf_

:: FormState 	= OldState !FState								// Old states are the states from the previous calculation
				| NewState !FState 								// New states are newly created states or old states that have been inspected and updated
:: FState		= { format	:: !Format							// Encoding method used for serialization
				  , life	:: !Lifespan						// Its life span
				  }
:: Format		= PlainStr 	!.String 							// Either a string is used for serialization
				| StatDyn	!Dynamic 							// Or a dynamic which enables serialization of functions defined in the application (no plug ins yet)
				| DBStr		.String (*Gerda -> *Gerda)			// In case a new value has to be stored in the relational database 
				| CLDBStr   .String (*DataFile -> *DataFile)	// In case a new value has to be stored in a Cleans database file
				
// Database OPTION

readGerda` id gerda 
:== IF_Database (readGerda id gerda)
					(abort "Reading from relational Database, yet option is swiched off\n", 
					 abort "Reading from relational Database, yet option is swiched off\n")

writeGerda` id val 
:== IF_Database (writeGerda id val)
					(\_ -> abort "Writing to relational Database, yet option is swiched off\n")

deleteGerda` id gerda
:== IF_Database (deleteGerda id gerda)
					(abort "Deleting data from Database, yet option is swiched off\n")


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
	= { fstates		= fstates	
	  , updates		= updates
	  , instates	= states
	  }
where
	fstates 
		= balance (sort [(formid, OldState {format = toExistval format state, life = lifespan}) 
						\\ {formid,lifespan, state, format} <- states
						|  formid <> ""
						])
	where
		toExistval PlainString   string	= PlainStr string						// string that has to be parsed in the context where the type is known
		toExistval StaticDynamic string	= StatDyn (string_to_dynamic` string)	// recover the dynamic


string_to_dynamic` :: !{#Char} -> Dynamic	// just to make a unique copy as requested by string_to_dynamic
string_to_dynamic` s	= string_to_dynamic {s` \\ s` <-: s}


getFormUpdates :: !String !*FormStates -> (![FormUpdate], !*FormStates)
getFormUpdates id formstates =: {updates} = ([update \\ update =:{FormUpdate|formid} <- updates | id == formid],formstates)

getAllUpdates :: !*FormStates -> (![FormUpdate], !*FormStates)
getAllUpdates formstates =: {updates} = (updates, formstates)

getUpdatedIds :: !*FormStates -> (![String],!*FormStates)
getUpdatedIds formstates=:{updates} = (removeDup [formid \\ {FormUpdate|formid} <- updates] ,formstates)


getState :: !(FormId a) !*FormStates !*NWorld -> (!Bool,!Maybe a,!*FormStates,!*NWorld)	| iPrint, iParse, iSpecialStore a	
getState formid formstates=:{fstates} world
# (bool,ma,fstates,world) = findState formid fstates world
= (bool,ma,{formstates & fstates = fstates},world)
where
	findState formid formstate=:(Node_ left (fid,info) right) world
	| formid.id == fid	= case info of
							(OldState state)	= (True, fetchFState state,formstate,world)
							(NewState state)	= (False,fetchFState state,formstate,world)
	with
		fetchFState :: FState -> Maybe a | TC a & gParse{|*|} a
		fetchFState {FState | format = PlainStr string}	= parseString string
		fetchFState {FState | format = DBStr string _}	= parseString string
		fetchFState {FState | format = CLDBStr string _}	= parseString string
		fetchFState {FState | format = StatDyn (v::a^)}	= Just v    
		fetchFState _							= Nothing
	| formid.id  < fid 	= (bool,parsed, Node_ leftformstates (fid,info) right,nworld)
						with
							(bool,parsed,leftformstates,nworld)  = findState formid left world
	| otherwise			= (bool,parsed, Node_  left (fid,info) rightformstates,nworld)
						with
							(bool,parsed,rightformstates,nworld) = findState formid right world

	// value is not yet available in the tree storage...
	// all stuff read out from persistent store is now marked as OldState (was NewState)	

	// read out relational Database and store as string 

	findState {id,lifespan = LSDatabase,storage = PlainString} Leaf_ world=:{gerda} 
	# (value,gerda)		= readGerda` id	gerda
	# world				= {world & gerda = gerda}
	= case value of
		Just a			= (True, Just a, Node_ Leaf_ (id,OldState {format = PlainStr (printToString a), life = LSDatabase}) Leaf_,world)
		Nothing			= (False,Nothing,Leaf_,world)

	// read out relational Database and store as dynamic

	findState {id,lifespan = LSDatabase,storage = StaticDynamic} Leaf_ world=:{gerda} 
	# (value,gerda)		= readGerda` id	gerda
	# world				= {world & gerda = gerda}
	= case value of 
		Nothing 		= (False,Nothing,Leaf_,world)
		Just string		= case string_to_dynamic` string of
							dyn=:(dynval::a^) 	-> (True, Just dynval,Node_ Leaf_ (id,OldState {format = StatDyn dyn, life = LSDatabase}) Leaf_,world)
							else				-> (False,Nothing,    Leaf_,world)

	// read out DataFile and store as string 

	findState {id,lifespan = LSDataFile,storage = PlainString} Leaf_ world=:{datafile} 
	# (value,datafile)	= readDataFile id datafile
	# world				= {world & datafile = datafile}
	= case value of
		Just a			= (True, Just a, Node_ Leaf_ (id,OldState {format = PlainStr (printToString a), life = LSDataFile}) Leaf_,world)
		Nothing			= (False,Nothing,Leaf_,world)

	// read out DataFile and store as dynamic

	findState {id,lifespan = LSDataFile,storage = StaticDynamic} Leaf_ world=:{datafile} 
	# (value,datafile)	= readDataFile id datafile
	# world				= {world & datafile = datafile}
	= case value of 
		Nothing 		= (False,Nothing,Leaf_,world)
		Just string		= case string_to_dynamic` string of
							dyn=:(dynval::a^) 	-> (True, Just dynval,Node_ Leaf_ (id,OldState {format = StatDyn dyn, life = LSDataFile}) Leaf_,world)
							else				-> (False,Nothing,    Leaf_,world)
	// read out file and store as string

	findState {id,lifespan = LSTxtFile,storage = PlainString} Leaf_ world 
	# (string,world)	= IF_Client ("",world) (readStateFile id world)
	= case parseString string of
		Just a			= (True, Just a, Node_ Leaf_ (id,OldState {format = PlainStr string, life = LSTxtFile}) Leaf_,world)
		Nothing			= (False,Nothing,Leaf_,world)

	findState {id,lifespan = LSTxtFileRO,storage = PlainString} Leaf_ world 
	# (string,world)	= IF_Client ("",world) (readStateFile id world)
	= case parseString string of
		Just a			= (True, Just a, Node_ Leaf_ (id,OldState {format = PlainStr string, life = LSTxtFileRO}) Leaf_,world)
		Nothing			= (False,Nothing,Leaf_,world)

	// read out file and store as dynamic

	findState {id,lifespan = LSTxtFile,storage = StaticDynamic} Leaf_ world 
	# (string,world)	= IF_Client ("",world) (readStateFile id world)
	= case string of 
		""				= (False,Nothing,Leaf_,world)
		_				= case string_to_dynamic` string of
							dyn=:(dynval::a^)	= (True, Just dynval,Node_ Leaf_ (id,OldState {format = StatDyn dyn, life = LSTxtFile}) Leaf_,world)
							else				= (False,Nothing,    Leaf_,world)

	findState {id,lifespan = LSTxtFileRO,storage = StaticDynamic} Leaf_ world 
	# (string,world)	= IF_Client ("",world) (readStateFile id world)
	= case string of 
		""				= (False,Nothing,Leaf_,world)
		_				= case string_to_dynamic` string of
							dyn=:(dynval::a^)	= (True, Just dynval,Node_ Leaf_ (id,OldState {format = StatDyn dyn, life = LSTxtFileRO}) Leaf_,world)
							else				= (False,Nothing,    Leaf_,world)

	// cannot find the value at all
	findState _ Leaf_ world	= (False,Nothing,Leaf_,world)
	findState _ _ world		= (False,Nothing,Leaf_,world)

setState ::  !(FormId a) a !*FormStates !*NWorld -> (!*FormStates,!*NWorld)	| iPrint,iSpecialStore a	
setState formid val formstates=:{fstates} world
# (fstates,world)		= replaceState formid val fstates world
= ({formstates & fstates = fstates},world)
where
	replaceState ::  !(FormId a) a *FStates *NWorld -> (*FStates,*NWorld)	| iPrint, iSpecialStore a	
	replaceState formid val Leaf_ world 									// id not part of tree yet
						= (Node_ Leaf_ (formid.id,NewState (initNewState formid.id (adjustlife formid.FormId.lifespan) LSTemp formid.storage val)) Leaf_,world)
	replaceState formid val (Node_ left a=:(fid,fstate) right) world
	| formid.id == fid	= (Node_ left (fid,NewState (initNewState formid.id formid.FormId.lifespan (detlifespan fstate) formid.storage val)) right,world)
	| formid.id <  fid	= (Node_ nleft a right,nworld)
							with
								(nleft, nworld) = replaceState formid val left  world
	| otherwise			= (Node_ left a nright,nworld)
							with
								(nright,nworld) = replaceState formid val right world

	// NewState Handling routines 

	initNewState :: !String !Lifespan !Lifespan !StorageFormat !a  -> FState | iPrint,  iSpecialStore a	
	initNewState id LSDatabase olifespan PlainString   nv = {format = DBStr    (printToString nv) (writeGerda`   id nv), 	life = order LSDatabase olifespan}
	initNewState id LSDataFile olifespan PlainString   nv = {format = CLDBStr  (printToString nv) (writeDataFile id nv), 	life = order LSDataFile olifespan}
	initNewState id lifespan olifespan PlainString   nv = {format = PlainStr (printToString nv),                     		life = order lifespan olifespan}
	initNewState id lifespan olifespan StaticDynamic nv = {format = StatDyn  (dynamic nv),                           		life = order lifespan olifespan}// convert the hidden state information stored in the html page

	adjustlife LSTxtFileRO 	= LSTxtFile		// to enforce that a read only persistent file is written once
	adjustlife life			= life

	detlifespan (OldState formstate) = formstate.life
	detlifespan (NewState formstate) = formstate.life

	order l1 l2			= if (l1 < l2) l2 l1	// longest lifetime chosen will be the final setting Database > DataFile > TxtFile > Session > Page > temp

deleteStates :: !String !*FormStates !*NWorld -> (!*FormStates,!*NWorld)	
deleteStates prefix formstates=:{fstates} world
# (fstates,world)		= deleteStates` fstates world
= ({formstates & fstates = fstates},world)
where
	lprefix 	= size prefix		

	deleteStates` :: !*FStates !*NWorld -> (!*FStates,!*NWorld)	
	deleteStates` Leaf_ world 			= (Leaf_,world)
	deleteStates` (Node_ left a=:(fid,_) right) world
	# prefid			= if (size fid <= lprefix) fid (fid%(0,lprefix-1))  // determine prefix of this form
	# lessegthen		= if (prefid == prefix) 0 (if (prefix < prefid) -1 1)
	# (nleft, world) 	= if (lessegthen <= 0) (deleteStates` left  world) (left,world)
	# (nright,world)	= if (lessegthen >= 0) (deleteStates` right world) (right,world)  
	| prefid == prefix	= deleteIData nleft nright a world
	= (Node_ nleft a nright,world)
	
	deleteIData left right a world
	# world = deletePersistentStorageIData a world
	= (join left right,world)
	where
		join Leaf_  right 	= right
		join left   Leaf_  	= left    
		join left   right	= Node_ nleft largest right
		where
			(largest,nleft)	= FindRemoveLargest left
	
			FindRemoveLargest (Node_ left x Leaf_)  = (x,left)
			FindRemoveLargest (Node_ left x right ) = (largest,Node_ left x nright)
			where
				(largest,nright) = FindRemoveLargest right

		deletePersistentStorageIData (fid,OldState {life}) world 	= deleteStorage fid life world
		deletePersistentStorageIData (fid,NewState {life}) world 	= deleteStorage fid life world

		deleteStorage fid LSDatabase 	world=:{gerda}		= {world & gerda  	 = deleteGerda`    fid gerda}
		deleteStorage fid LSDataFile 	world=:{datafile}	= {world & datafile  = deleteDataFile fid datafile}
		deleteStorage fid LSTxtFile 	world 				= deleteStateFile fid world
		deleteStorage fid LSTxtFileRO 	world				= deleteStateFile fid world
		deleteStorage fid _ 			world 				= world

// change storage option

changeLifetimeStates :: !String !Lifespan !Lifespan !*FormStates !*NWorld -> (!*FormStates,!*NWorld)	
changeLifetimeStates prefix oldlifespan newlifespan formstates=:{fstates} world
# (fstates,world)		= changeLifetimeStates` fstates world
= ({formstates & fstates = fstates},world)
where
	lprefix 	= size prefix		

	changeLifetimeStates` :: !*FStates !*NWorld -> (!*FStates,!*NWorld)	
	changeLifetimeStates` Leaf_ world 			= (Leaf_,world)
	changeLifetimeStates` (Node_ left a=:(fid,_) right) world
	# prefid			= if (size fid <= lprefix) fid (fid%(0,lprefix-1))  // determine prefix of this form
	# lessegthen		= if (prefid == prefix) 0 (if (prefix < prefid) -1 1)
	# (nleft, world) 	= if (lessegthen <= 0) (changeLifetimeStates` left  world) (left,world)
	# (nright,world)	= if (lessegthen >= 0) (changeLifetimeStates` right world) (right,world)  
	| prefid == prefix	= changeLifetime nleft nright a world
	= (Node_ nleft a nright,world)
	
	changeLifetime left right a=:(fid,OldState fstate=:{life}) world
	| life == oldlifespan	= (Node_ left (fid,OldState {fstate & life = newlifespan}) right,world)	
	= (Node_ left a right,world)
	changeLifetime left right a=:(fid,NewState fstate=:{life}) world
	| life == oldlifespan	= (Node_ left (fid,NewState {fstate & life = newlifespan}) right,world)	
	= (Node_ left a right,world)

getHtmlStates :: !*FormStates -> (![HtmlState], !*FormStates)
getHtmlStates formstates =: {fstates}
	# (fstates, htmlstates) = filterHtmlStates fstates [] 
	= (htmlstates, {formstates & fstates = fstates})
where
	filterHtmlStates Leaf_ accu	= (Leaf_, accu)
	filterHtmlStates (Node_ left x right) accu
		# (left,accu)	= filterHtmlStates left accu
		= case htmlStateOf x of
			Nothing
				# (right,accu)	= filterHtmlStates right accu
				= (Node_ left x right, accu)	
			Just state
				# (right,accu)	= filterHtmlStates right [state:accu]
				= (Node_ left x right, accu)		
	where
		// old states which have not been used this time, but with lifespan session, are stored again in the page
		// other old states will have lifespan page or are persistent; they need not to be stored
		htmlStateOf (fid,OldState {life=LSSession,format=PlainStr stringval})	= Just  {HtmlState|formid=fid, lifespan=LSSession, state=stringval, format=PlainString}
		htmlStateOf (fid,OldState {life=LSSession,format=StatDyn  dynval})		= Just  {HtmlState|formid=fid, lifespan=LSSession, state=dynamic_to_string dynval, format=StaticDynamic}

		htmlStateOf (fid,OldState {life=LSClient, format=PlainStr stringval})	= Just  {HtmlState|formid=fid, lifespan=LSClient, state=stringval, format=PlainString}
		htmlStateOf (fid,OldState {life=LSClient, format=StatDyn  dynval})		= Just  {HtmlState|formid=fid, lifespan=LSClient, state=dynamic_to_string dynval, format=StaticDynamic}		

		htmlStateOf (fid,OldState s)										= Nothing

		// persistent stores (either old or new) have already been stored in files and can be skipped here
		// temperal form don't need to be stored and can be skipped as well
		// the state of all other new forms created are stored in the page 
		htmlStateOf (fid,NewState {life})
			| isMember life [LSDatabase,LSTxtFile,LSTxtFileRO,LSDataFile,LSTemp]	= Nothing

		htmlStateOf (fid,NewState {format = PlainStr string,life})			= Just {HtmlState|formid=fid, lifespan=life, state=string, format=PlainString}
		htmlStateOf (fid,NewState {format = StatDyn dynval, life})			= Just {HtmlState|formid=fid, lifespan=life, state=dynamic_to_string dynval, format=StaticDynamic}

		htmlStateOf _														= Nothing

storeServerStates :: !*FormStates !*NWorld -> (!*FormStates, !*NWorld)
storeServerStates formstates =:{fstates} nworld
	# (fstates, nworld) = writeStates fstates nworld
	= ({formstates & fstates = fstates},nworld)
where
	writeStates Leaf_ nworld
		= (Leaf_, nworld)
	writeStates (Node_ left st right) nworld
		# (left, nworld)	= writeStates left nworld
		# nworld			= writeState st nworld
		# (right, nworld)	= writeStates right nworld
		= (Node_ left st right, nworld)
	
	// only new states need to be stored, since old states have not been changed (assertion)
	writeState (sid,NewState {format,life = LSDatabase}) nworld=:{gerda}
		= case format of
			DBStr   string gerdafun		= {nworld & gerda = gerdafun gerda}										// last value is stored in curried write function
			StatDyn dynval				= {nworld & gerda = writeGerda` sid (dynamic_to_string dynval) gerda}	// write the dynamic as a string to the relational database
	writeState (sid,NewState {format,life = LSDataFile}) nworld=:{datafile}
		= case format of
			CLDBStr   string dfilefun	= {nworld & datafile = dfilefun datafile}										// last value is stored in curried write function
			StatDyn dynval				= {nworld & datafile = writeDataFile sid (dynamic_to_string dynval) datafile}	// write the dynamic as a string to the datafile
	writeState (sid,NewState {format,life  = LSTxtFile}) nworld
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
readStateFile  filename env
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
 
traceStates :: !*FormStates -> (!HtmlTag,!*FormStates)
traceStates formstates=:{fstates}
# (rows, fstates) = traceStates` fstates
= (DivTag [IdAttr "itasks-trace-states",ClassAttr "trace"] [H2Tag [] [Text "States:"], TableTag [] [header : rows]], {formstates & fstates = fstates})
where
	header = TrTag [] [ThTag [] [Text "Form ID"],ThTag [] [Text "Inspected"],ThTag [] [Text "Lifespan"],ThTag [] [Text "Format"], ThTag [] [Text "Value"]]

	traceStates` Leaf_		= ([],Leaf_)
	traceStates` (Node_ left a right)
	# (leftTrace,left)		= traceStates` left
	# nodeTrace				= nodeTrace a
	# (rightTrace,right)	= traceStates` right
	= (leftTrace ++ nodeTrace ++ rightTrace, Node_ left a right)

	nodeTrace (id,OldState fstate=:{format,life}) = [TrTag [] [TdTag [] [Text id], TdTag [] [Text "No"], TdTag [] [Text (toString life)]: toCells format] ]
	nodeTrace (id,NewState fstate=:{format,life}) = [TrTag [] [TdTag [] [Text id], TdTag [] [Text "Yes"], TdTag [] [Text (toString life)]: toCells format] ]
	
	toCells (PlainStr str) 		= [TdTag [] [Text "String"],TdTag [] [Text str]]
	toCells (StatDyn  dyn) 		= [TdTag [] [Text "S_Dynamic"],TdTag [] [Text (ShowValueDynamic dyn <+++ " :: " <+++ ShowTypeDynamic dyn )]]
	toCells (DBStr    str _) 	= [TdTag [] [Text "Database"],TdTag [] [Text str]]
	toCells (CLDBStr  str _) 	= [TdTag [] [Text "DataFile"],TdTag [] [Text str]]

ShowValueDynamic :: !Dynamic -> String
ShowValueDynamic d = strip (foldr (+++) "" (fst (toStringDynamic d)) +++ " ")

ShowTypeDynamic :: !Dynamic -> String
ShowTypeDynamic d = strip (snd (toStringDynamic d) +++ " ")

strip :: !String -> String
strip s = { ns \\ ns <-: s | ns >= '\020' && ns <= '\0200'}

traceUpdates :: !*FormStates -> (!HtmlTag,!*FormStates)
traceUpdates formstates =:{updates}
= (DivTag [IdAttr "itasks-trace-updates",ClassAttr "trace"] [H2Tag [] [Text "Updates:"], TableTag [] [header : rows]], formstates)
where
	header	= TrTag [] [ThTag [] [Text "Form ID"], ThTag [] [Text "Input ID"], ThTag [] [Text "Value"]]
	rows	= [TrTag [] [TdTag [] [Text formid], TdTag [] [Text (toString inputid)], TdTag [] [Text value]] \\ {FormUpdate|formid,inputid,value} <- updates ]

traceInStates	:: !*FormStates -> (!HtmlTag,!*FormStates)
traceInStates formstates =:{instates}
= (DivTag [IdAttr "itasks-trace-instates",ClassAttr "trace"] [H2Tag [] [Text "Initial Html States:"], TableTag [] [header : rows]],formstates)
where
	header	= TrTag [] [ThTag [] [Text "Form ID"], ThTag [] [Text "Lifespan"], ThTag [] [Text "Format"], ThTag [] [Text "Value"]]
	rows	= [TrTag [] [TdTag [] [Text formid], TdTag [] [Text (toString lifespan)], TdTag [] [Text (toString format)],TdTag [] [Text state]] \\ {HtmlState|formid,lifespan,state,format} <- instates]


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

//Create a balanced storage tree:
balance :: ![a] -> .(Tree_ a)
balance []					= Leaf_
balance [x]					= Node_ Leaf_ x Leaf_
balance xs
	= case splitAt (length xs/2) xs of
		(a,[b:bs])			= Node_ (balance a) b (balance bs)
		(as,[])				= Node_ (balance (init as)) (last as) Leaf_


// functions defined on the FormStates abstract data type

instance < FormState
where
	(<) _ _ = True

// interfaces added for testing:
import GenMap
derive gMap Tree_

initTestFormStates :: !*NWorld -> (!*FormStates,!*NWorld)													// retrieves all form states hidden in the html page
initTestFormStates world 
	= ({ fstates = Leaf_, updates = [], instates = []},world)

setTestFormStates :: ![FormUpdate] !String !String !*FormStates !*NWorld -> (!*FormStates,!*NWorld)			// retrieves all form states hidden in the html page
setTestFormStates updates updateid update states world 
	= ({ fstates = gMap{|*->*|} toOldState states.fstates, updates = updates, instates = []},world)
where
	toOldState (s,NewState fstate)	= (s,OldState fstate)
	toOldState else					= else
	

