implementation module iDataHandler

import StdArray, StdChar, StdList, StdStrictLists, StdString, StdTuple
import ArgEnv, StdMaybe
import iDataHtmlDef, iDataTrivial, iDataSettings, iDataStylelib, iDataState
import StdGeneric, GenParse, GenPrint
import httpServer, httpSubServer
import Gerda
import StdBimap

derive gPrint (,), (,,), (,,,), UpdValue
derive gParse (,), (,,), (,,,), UpdValue
derive gHpr   (,), (,,), (,,,)
derive gUpd		   (,,), (,,,)
derive bimap Form, FormId

gParse{|(->)|} gArg gRes _ 		= Nothing 
gPrint{|(->)|} gArg gRes _ _	= abort "functions can only be used with dynamic storage option!\n" 

:: *HSt 		= { cntr 	:: Int 			// counts position in expression
				  , submits	:: Bool			// True if we are in submit form
				  , states	:: *FormStates  // all form states are collected here ... 	
				  , world	:: *NWorld		// to enable all other kinds of I/O
				  }	
:: InputId	 	:== Int						// unique id for every constructor and basic value appearing in the state
:: FormUpdate	:== (InputId,UpdValue)		// info obtained when form is updated

:: Inline 		= Inline String

// Database OPTION

openDatabase database world
:== IF_Database (openGerda database world) (abort "Trying to open a relational database while this option is switched off",world)
closeDatabase database world
:== IF_Database (closeGerda database world) world

// DataFile OPTION

openmDataFile datafile world
:== IF_DataFile (openDataFile  datafile world) (abort "Trying to open a dataFile while this option is switched off",world)
closemDataFile datafile world
:== IF_DataFile (closeDataFile datafile world) world

//////////////////  EXPERIMENTAL

doHtmlServer2 :: ![(String,*HSt -> (Html,!*HSt))] !*World -> *World
doHtmlServer2 userpages world
| ServerKind == Internal		// link in http 1.0 server
= StartServer SocketNr [(thisExe, \_ _ args -> doHtmlPageAndPrint args userpage) \\ (thisExe,userpage) <- userpages] world
//| ServerKind == External		// connect with http 1.1 server
//= doHtmlSubServer userpage world

// Experimental version of doHtmlServer for Client site evaluation ....

doHtmlClient :: !(*HSt -> (Html,!*HSt)) !*World -> *World
doHtmlClient userpage world
= callSapl (\args ->  doHtmlPageAndPrint args userpage) world

callSapl :: ([(String, String)] *World -> ([String],String,*World)) !*World -> *World
callSapl f world 
# (_,allhtmlcode,world) = f [] world
= abort allhtmlcode
//= world

// doMyHtmlServerAjax has an extra function in the list for the default case
// the other one generates the inner html


/////////////////////////////////


// doHtmlServer: top level function given to end user.
// It sets up the communication with a (sub)server, depending on the option chosen.

doHtmlServer :: !(*HSt -> (Html,!*HSt)) !*World -> *World
doHtmlServer userpage world
| ServerKind == Internal		// link in http 1.0 server
= IF_Ajax 
	(StartAjax   userpage world)
	(StartServer SocketNr [(ThisExe, \_ _ args -> doHtmlPageAndPrint args userpage)] world)
| ServerKind == External		// connect with http 1.1 server
= doHtmlSubServer userpage world

StartAjax :: !(*HSt -> (Html,!*HSt)) !*World -> *World
StartAjax userpage world
= StartServer SocketNr [(ThisExe, \_ _ args -> defaultpage args),  // empty page with script and div
                        (ThisExe +++ "_ajax", \_ _ args -> doHtmlPageAndPrint args userpage)] world
where
	defaultpage  _ world =  ([], page,world)
	where  page = 	"<html>" +++
						"<head>" +++
							"<link type=\"text/css\" rel=\"stylesheet\" href=\"" +++ ThisExe +++ "/clean.css\" />" +++		// clean styles now code in sepparate style sheet
							"<script  language=\"JavaScript\" src=\"" +++ ThisExe +++ "/ajaxscript.js\"></script>" +++		// script for handling ajax code
						"</head>" +++
	             		 "<body background = " +++ ThisExe +++ "/back35.jpg class = CleanStyle>" +++ 
	             		 "<div id=\"thePage\" class=\"thread\">" +++ ThisExe +++ "</div>" +++ 
	             		 "<div id=\"theState\" class=\"thread\"></div>" +++ 
	             		 "<div id=\"debug\" class=\"thread\"></div>" +++ 
	               		 "</body>" +++
	              	"</html>"

// doHtmlPageAndPrint is the main driver shared by all server options.
// It initiates all internal administration and calls the user defined iData or iTask function userpage.
// It converts the Html value into html code which is handed over to the server.  

doHtmlPageAndPrint :: [(String, String)] .(*HSt -> (Html,!*HSt)) *World -> ([String],String,*World)
doHtmlPageAndPrint args userpage world
# (inout,world)			= doHtmlPage (Just args) userpage [|] world
# n_chars				= count_chars inout 0
# allhtmlcode			= copy_strings inout n_chars (createArray n_chars '\0')
= ([],allhtmlcode,world)
where
	doHtmlPage ::  !(Maybe [(String, String)]) !.(*HSt -> (Html,!*HSt)) !*HtmlStream !*World -> (!*HtmlStream,!*World)
	doHtmlPage args userpage inout world
	# (gerda,world)				= openDatabase ODCBDataBaseName world						// open the relational database if option chosen
	# (datafile,world)			= openmDataFile DataFileName world							// open the datafile if option chosen
	# nworld 					= {worldC = world, inout = inout, gerda = gerda, datafile = datafile}	
	# (initforms,nworld)	 	= retrieveFormStates args nworld							// Retrieve the state information stored in an html page, other state information is collected lazily
	# (Html (Head headattr headtags) (Body attr bodytags),{states,world}) 
								= userpage (mkHSt initforms nworld)							// Call the user application
	# (debugOutput,states)		= if TraceOutput (traceStates states) (EmptyBody,states)	// Optional show debug information
	# (allformbodies,world=:{worldC,gerda,inout,datafile})	
								= storeFormStates states world								// Store all state information
	# worldC					= closeDatabase gerda worldC								// close the relational database if option chosen
	# worldC					= closemDataFile datafile worldC							// close the datafile if option chosen
	# inout						= IF_Ajax
									(print_to_stdout "" inout <+
									allformbodies <+ State_FormList_Separator <+			// state information
            		 				AjaxCombine bodytags [debugInput,debugOutput]												// page, or part of a page
									)
									(print_to_stdout 										// Print out all html code
										(Html (Head headattr [extra_style:headtags]) 
										(Body (extra_body_attr ++ attr) [allformbodies:bodytags++[debugInput,debugOutput]]))
										inout
									)
	= (inout,worldC)
	where
		AjaxCombine [Ajax bodytags:ys] [EmptyBody,EmptyBody] 	= [Ajax bodytags:ys]
		AjaxCombine [Ajax bodytags:ys] debug 	= [Ajax [("debug",debug):bodytags]:ys]
		
		extra_body_attr			= [Batt_background (ThisExe +++ "/back35.jpg"),`Batt_Std [CleanStyle]]
		extra_style				= Hd_Style [] CleanStyles	
		debugInput				= if TraceInput (traceHtmlInput args) EmptyBody

	count_chars [|]    n = n
	count_chars [|s:l] n = count_chars l (n+size s)

	copy_strings [|e:l] i s
		# size_e	= size e
		# i			= i-size_e
		= copy_strings l i (copy_chars e 0 i size_e s)
	copy_strings [|] 0 s
		= s

	copy_chars :: !{#Char} !Int !Int !Int !*{#Char} -> *{#Char}
	copy_chars s_s s_i d_i n d_s
		| s_i<n
			# d_s	= {d_s & [d_i]=s_s.[s_i]}
			= copy_chars s_s (s_i+1) (d_i+1) n d_s
			= d_s

mkHSt :: *FormStates *NWorld -> *HSt
mkHSt states nworld = {cntr=0, states=states, world=nworld, submits = False }

// Create subserver(s) talking to an http 1.1 server.
// One needs to create several copies of the same subserver to handle parallel request issues by an http 1.1 server.
// To prevent race conditions, calls to such a subserver copy is serialized using a semaphore. 

import Semaphore	

doHtmlSubServer :: !(*HSt -> (Html,!*HSt)) !*World -> *World
doHtmlSubServer userpage world
	# result = RegisterSubProcToServer 1 0 100 ".*" (ThisExe +++ ".*")
	| result == 1
		# (console,world) = stdio world
		# (_,world) = fclose (fwrites ("Error: SubServer \"" +++ location +++ "\" could *NOT* registered to an HTTP 1.1 main server\n") console) world
		= world
	| result == 2
		# (console,world) = stdio world
		# (_,world) = fclose (fwrites ("SubServer \"" +++ location +++ "\" successfully registered to an HTTP 1.1 main server\n") console) world
		= world
	# (semaphore,world) = CreateSemaphore 0 1 1 ThisExe world
	| semaphore == 0	= abort "CreateSemaphore failed"
	# world 			= WaitForMessageLoop (mycallbackfun semaphore) SocketNr world
	# (ok,world) 		= CloseHandle semaphore world
	| ok==0				= abort "CloseHandle failed"
	= world
where
	mycallbackfun :: !Int [String] Int Socket *World -> (Socket,*World)
	mycallbackfun semaphore header contentlength socket world
	# (method,rlocation,getDataArray,version) 		= GetFirstLine (hd header)
	# (alldatareceived,datafromclient,socket,world)	= ReceiveString 0 contentlength socket world
	| socket==0 						= (0,world)				//socket closed or timed out
	| alldatareceived == -1 && location == rlocation
		#! world = trace ("alldatareceived == -1, page request" +++ ";rloc=" +++ rlocation ) world
		#! (_,htmlcode,world) 				= indivisable (doHtmlPageAndPrint [] userpage) world
		= SendString htmlcode "text/html" header socket world
	| alldatareceived == -1 && location <> rlocation
		#! world = trace ("alldatareceived == -1, file request" +++ ";rloc=" +++ rlocation ) world
		= SendFile MyAbsDir header socket world 
	| alldatareceived <> 0
		#! world = trace ("alldatareceived <> 0, more data requested... cannot handle this" +++ ";rloc=" +++ rlocation ) world
		= SendString "Unexpected request " "text/plain" header socket world
	| alldatareceived == 0 && rlocation <> location 			// server asks for files
			#! world = trace ("alldatareceived == 0, file request" +++ ";rloc=" +++ rlocation ) world
			= SendFile (MyAbsDir +++ rlocation) header socket world 
	# (_,htmlcode,world) 	= indivisable (doHtmlPageAndPrint (makeArguments datafromclient) userpage) world
	#! world = trace ("alldatareceived == 0,  page request" +++ ";rloc=" +++ rlocation ) world
	= SendString htmlcode "text/html" header socket world
	where
		indivisable doServer world
		# (_,world) 			= WaitForSingleObject semaphore -1 world
		# (r,htmlcode,world) 	= doServer world
		# (ok,world) 			= ReleaseSemaphore semaphore 1 0 world
		= (r,htmlcode,world)
		
	trace s world = if TraceHttp11 (trace_to_file s world) world
	location = "\/" +++ ThisExe

// The function mkViewForm is *the* magic main function 
// All idata /itasks end up in this function
// It does everything, a swiss army knife editor that makes coffee too ...

mkViewForm :: !(InIDataId d) !(HBimap d v) !*HSt -> (Form d,!*HSt) | iData v
mkViewForm (init,formid) bm=:{toForm, updForm, fromForm, resetForm} hst=:{states,world,submits} 
| init == Const	&& formid.lifespan <> Temp
= mkViewForm (init,{formid & lifespan = Temp}) bm hst					// constant i-data are never stored
| init == Const															// constant i-data, no look up of previous value
= calcnextView False Nothing states world				
# (isupdated,view,states,world) = findFormInfo vformid states world 	// determine current view value in the state store
= calcnextView isupdated view states world								// and calculate new i-data
where
	vformid					= reuseFormId formid (toForm init formid.ival Nothing)

	calcnextView isupdated view states world
	# (changedids,states)	= getUpdateId states
	# changed				= {isChanged = isupdated, changedId = changedids}
	# view					= toForm init formid.ival view				// map value to view domain, given previous view value
	# view					= updForm  changed view						// apply update function telling user if an update has taken place
	# newval				= fromForm changed view						// convert back to data domain	 
	# view					= case resetForm of							// optionally reset the view hereafter for next time
								Nothing 	-> view		 
								Just reset 	-> reset view

	| formid.mode == NoForm												// don't make a form at all
		# (states,world)	= replaceState` vformid view states world	// store new value into the store of states
		= ({ changed		= False
		   , value			= newval
		   , form			= []
		   }
		  ,mkHSt states world)

	# (viewform,{states,world})											// make a form for it
							= mkForm (init,if (init == Const) vformid (reuseFormId formid view)) ({mkHSt states world & submits = submits})

	| viewform.changed && not isupdated						 			// important: redo it all to handle the case that a user defined specialisation is updated !!
							= calcnextView True (Just viewform.value) states world

	# (states,world)		= replaceState` vformid viewform.value states world	// store new value into the store of states

	= (	{ changed			= isupdated
		, value				= newval
		, form				= viewform.form
		}
	  ,mkHSt states world)

	replaceState` vformid view states world
	| init <> Const			= replaceState vformid view states world
	| otherwise				= (states,world)

//	findFormInfo :: FormId *FormStates *NWorld -> (Bool,Maybe a,*FormStates,*NWorld) | gUpd{|*|} a & gParse{|*|} a & TC a
	findFormInfo formid formStates world
	# (updateids,formStates) 					= getUpdateId formStates // get list of updated id's
	| not (isMember formid.id updateids)		
		# (bool,justcurstate,formStates,world)	= findState formid formStates world									// the current form is not updated
		= (False,justcurstate,formStates,world)
	# (alltriplets,formStates)	= getTriplets formid.id formStates		// get my update triplets
	= case (findState formid formStates world) of
			(False,Just currentState,formStates,world) -> (False, Just currentState,formStates,world) 				// yes, but update already handled
			(True, Just currentState,formStates,world) -> updateState alltriplets currentState formStates world		// yes, handle update
			(_,    Nothing,formStates,world) 		   -> (False, Nothing,formStates,world) 		  				// cannot find previously stored state


	updateState alltriplets currentState formStates world
	# allUpdates = [update \\ tripletupd <- alltriplets, (Just update) <- [examineTriplet tripletupd]]
	# newState = applyUpdates allUpdates currentState
	= (True,Just newState,formStates,world)

	applyUpdates [] currentState 					= currentState
	applyUpdates [(pos,upd):updates] currentState	= applyUpdates updates (snd (gUpd{|*|} (UpdSearch upd pos) currentState))

	examineTriplet :: TripletUpdate -> Maybe (Int,UpdValue)
	examineTriplet tripletupd
		= case parseTriplet tripletupd of
			((sid,pos,UpdC s), Just "") 								= (Just (pos,UpdC s) )
			((sid,pos,UpdC s), _) 										= (Just (pos,UpdC s) )
			(_,_)= case parseTriplet tripletupd of
					((sid,pos,UpdI i), Just ni) 						= (Just (pos,UpdI ni))
					((sid,pos,UpdI i), _) 								= (Just (pos,UpdI i) )
					(_,_) = case parseTriplet tripletupd of
							((sid,pos,UpdR r), Just nr) 				= (Just (pos,UpdR nr))
							((sid,pos,UpdR r), _) 						= (Just (pos,UpdR r) )
							(_,_) = case parseTriplet tripletupd of
									((sid,pos,UpdB b), Just nb) 		= (Just (pos,UpdB nb))
									((sid,pos,UpdB b), _) 				= (Just (pos,UpdB b) )
									(_,_) = case parseTriplet tripletupd of
										((sid,pos,UpdS s),	Just ns)	= (Just (pos,UpdS ns))
										_								= case  tripletupd of
																			((sid,pos,UpdS s), ns) -> (Just (pos, UpdS ns))
																			_ -> (Nothing			 )
	where
		parseTriplet :: TripletUpdate -> (Triplet,Maybe b) | gParse {|*|} b
		parseTriplet (triplet,update) = (triplet,parseString update)

// It can be convenient to explicitly delete IData, in particular for persistent IData obejct
// or to optimize iTasks
// All IData objects administrated in the state satisfying the predicate will be deleted, no matter where they are stored.

deleteIData :: !String !*HSt -> *HSt
deleteIData prefix hst=:{states,world}
# (states,world) = deleteStates prefix states world
= {hst & states = states, world = world}

// specialize has to be used if a programmer wants to specialize gForm.
// It remembers the current value of the index in the expression and creates an editor to show this value.
// The value might have been changed with this editor, so the value returned might differ from the value you started with!

specialize :: !((InIDataId a) *HSt -> (Form a,*HSt)) !(InIDataId a) !*HSt -> (!Form a,!*HSt) | gUpd {|*|} a
specialize editor (init,formid) hst=:{cntr = inidx,states = formStates,submits,world}
# nextidx					= incrIndex inidx formid.ival		// this value will be repesented differently, so increment counter 
# (nv,hst) 					= editor (init,nformid) (setCntr 0 hst)
= (nv,{setCntr nextidx hst & submits = submits})
where
	nformid					= {formid & id = formid.id <+++ "_specialize_" <+++ inidx <+++ "_"}

	incrIndex :: Int v -> Int | gUpd {|*|} v
	incrIndex i v
	# (UpdSearch _ cnt,v)	= gUpd {|*|} (UpdSearch (UpdI 0) -1) v
	= i + (-1 - cnt)

// gForm: automatically derives a Html form for any Clean type

mkForm :: !(InIDataId a) !*HSt -> *(Form a, !*HSt)	| gForm {|*|} a
mkForm (init,formid=:{mode = Submit}) hst=:{submits = False} 
# (form,hst) 	= gForm{|*|} (init,formid) {hst & submits = True}
# hst			= {hst & submits = False}
# hidden		= Input [ Inp_Name "hidden"
						, Inp_Type Inp_Hidden
						, Inp_Value (SV "")
						] ""
# submit		= Input [ Inp_Type Inp_Button
						, Inp_Value (SV "Submit")
						,`Inp_Events (callClean OnClick Submit formname)
						] ""
# clear			= Input [ Inp_Type Inp_Reset, Inp_Value (SV "Clear")] ""
# sform			= [Form [ Frm_Method Post
						, Frm_Name  formname
						] (form.form ++ [hidden,Br,submit,clear])
				  ] 
= ({form & form = sform} ,hst)
where
	formname = encodeString formid.id				// to enable the use of any character in a form name
mkForm inidataid hst = gForm{|*|} inidataid hst
	
generic gForm a :: !(InIDataId a) !*HSt -> *(Form a, !*HSt)	

gForm{|Int|} (init,formid) hst 	
# (body,hst)				= mkInput defsize (init,formid) (IV i) (UpdI i) hst
= ({ changed				= False
   , value					= i
   , form					= [body]
   },hst)
where
	i						= formid.ival 
gForm{|Real|} (init,formid) hst 	
# (body,hst)				= mkInput defsize (init,formid) (RV r) (UpdR r) hst
= ({ changed				= False
   , value					= r
   , form					= [body]
   },hst)
where
	r						= formid.ival 
gForm{|Bool|} (init,formid) hst 	
# (body,hst)				= mkInput defsize (init,formid) (BV b) (UpdB b) hst
= ({ changed				= False
   , value					= b
   , form					= [body]
   },hst)
where
	b						= formid.ival 
gForm{|String|} (init,formid) hst 	
# (body,hst)				= mkInput defsize (init,formid) (SV s) (UpdS s) hst
= ({ changed				= False
   , value					= s
   , form					= [body]
   },hst)
where
	s						= formid.ival 
gForm{|UNIT|}  _ hst
= ({ changed				= False
   , value					= UNIT
   , form					= [EmptyBody]
   },hst)
gForm{|PAIR|} gHa gHb (init,formid) hst 
# (na,hst)					= gHa (init,reuseFormId formid a) hst
# (nb,hst)					= gHb (init,reuseFormId formid b) hst
= ({ changed				= na.changed || nb.changed
   , value					= PAIR na.value nb.value
   , form					= [STable [Tbl_CellPadding (Pixels 0), Tbl_CellSpacing (Pixels 0)] [na.form,nb.form]]
   },hst)
where
	(PAIR a b)				= formid.ival 
gForm{|EITHER|} gHa gHb (init,formid)  hst 
= case formid.ival of
	(LEFT a)
		# (na,hst)			= gHa (init,reuseFormId formid a) hst
		= ({na & value=LEFT na.value},hst)
	(RIGHT b)
		# (nb,hst)			= gHb (init,reuseFormId formid b) hst
		= ({nb & value=RIGHT nb.value},hst)
gForm{|OBJECT|} gHo (init,formid) hst
# (no,hst)					= gHo (init,reuseFormId formid o) hst
= ({no & value=OBJECT no.value},hst)
where
	(OBJECT o) = formid.ival
gForm{|CONS of t|} gHc (init,formid) hst=:{cntr,submits}
| not (isEmpty t.gcd_fields) 		 
	# (nc,hst)				= gHc (init,reuseFormId formid c) (setCntr (cntr+1) hst) // don't display record constructor
	= ({nc & value=CONS nc.value},hst)
| t.gcd_type_def.gtd_num_conses == 1 
	# (nc,hst)				= gHc (init,reuseFormId formid c) (setCntr (cntr+1) hst) // don't display constructors that have no alternative
	= ({nc & value=CONS nc.value},hst)
| t.gcd_name.[(size t.gcd_name) - 1] == '_' 										 // don't display constructor names which end with an underscore
	# (nc,hst)				= gHc (init,reuseFormId formid c) (setCntr (cntr+1) hst) 
	= ({nc & value=CONS nc.value},hst)
# (selector,hst)			= mkConsSelector formid t hst
# (nc,hst)					= gHc (init,reuseFormId formid c) hst
= ({ changed				= nc.changed
   , value					= CONS nc.value
   , form					= [STable [Tbl_CellPadding (Pixels 0), Tbl_CellSpacing (Pixels 0)] [[selector,BodyTag nc.form]]]
   },hst)
where
	(CONS c)				= formid.ival

	mkConsSelector formid thiscons hst=:{cntr} 
							= (mkConsSel cntr myname allnames myindex formid, setCntr (cntr+1) hst)
	where
		myname				= thiscons.gcd_name
		allnames			= map (\n -> n.gcd_name) thiscons.gcd_type_def.gtd_conses
		myindex				= case allnames ?? myname of
								-1			-> abort ("cannot find index of " +++ myname )
								i			-> i

	mkConsSel :: Int String [String] Int (FormId x) -> BodyTag
	mkConsSel cntr myname list nr formid
		= Select [ Sel_Name (selectorInpName +++ encodeString myname) : styles ]		// changed to see changes in case of a submit
				 [ Option  
					[Opt_Value (encodeTriplet (formid.id,cntr,UpdC elem)) : if (j == nr) [Opt_Selected Selected:optionstyle] optionstyle] elem
				 \\ elem <- list & j <- [0..]
				 ] 
		where
			styles			= case formid.mode of
								Edit	-> [ `Sel_Std	[Std_Style width, EditBoxStyle]
										   , `Sel_Events (if submits [] (callClean OnChange Edit formid.id))
										   ]
								Submit	-> [ `Sel_Std	[Std_Style width, EditBoxStyle]
										   ]
								_		-> [ `Sel_Std	[Std_Style width, DisplayBoxStyle]
										   ,  Sel_Disabled Disabled
										   ]
			optionstyle		= case formid.mode of
								Edit	-> []
								Submit	-> []
								_	 	-> [`Opt_Std [DisplayBoxStyle]]

			width			= "width:" <+++ defpixel <+++ "px"
gForm{|FIELD of d |} gHx (init,formid) hst 
# (nx,hst)					= gHx (init,reuseFormId formid x) hst
= ({ changed				= nx.changed
   , value					= FIELD nx.value
   , form					= [STable [Tbl_CellPadding (Pixels 1), Tbl_CellSpacing (Pixels 1)] [[fieldname,BodyTag nx.form]]]
   },hst)
where
	(FIELD x)				= formid.ival
	
	fieldname				= Input [ Inp_Type		Inp_Text
									, Inp_Value		(SV (prettify d.gfd_name +++ ": "))
									, Inp_ReadOnly	ReadOnly
									, Inp_Disabled	Disabled
									, `Inp_Std		[DisplayBoxStyle]
									, Inp_Size		maxsize`
									] ""

	prettify name			= mkString [toUpper lname : addspace lnames]
	where
		[lname:lnames]		= mkList name
		addspace []			= []
		addspace [c:cs]
		| isUpper c			= [' ',toLower c:addspace cs]
		| otherwise			= [c:addspace cs]
		
	maxsize`				= ndefsize maxsize defsize
	maxsize					= takemax defsize [size (prettify gfd_name) * 8 / 10 \\ {gfd_name} <- d.gfd_cons.gcd_fields]
	
	takemax i []			= i
	takemax i [j:js] 
	| i > j					= takemax i js
	| otherwise				= takemax j js

	ndefsize max def
	| max - def <= 0		= def
	| otherwise				= ndefsize max (def + defsize)
gForm{|(->)|} garg gres (init,formid) hst 	
= ({ changed = False, value = formid.ival, form = []},hst)

// gUpd calculates a new value given the current value and a change in the value.
// If necessary it invents new default values (e.g. when switching from Nil to Cons)
// and leaves the rest untouched.

:: UpdMode	= UpdSearch UpdValue Int		// search for indicated postion and update it
			| UpdCreate [ConsPos]			// create new values if necessary
			| UpdDone						// and just copy the remaining stuff

generic gUpd t :: UpdMode t -> (UpdMode,t)

gUpd{|Int|} (UpdSearch (UpdI ni) 0) 	_ = (UpdDone,ni)					// update integer value
gUpd{|Int|} (UpdSearch val cnt)     	i = (UpdSearch val (dec cnt),i)		// continue search, don't change
gUpd{|Int|} (UpdCreate l)				_ = (UpdCreate l,0)					// create default value
gUpd{|Int|} mode 			  	    	i = (mode,i)						// don't change

gUpd{|Real|} (UpdSearch (UpdR nr) 0) 	_ = (UpdDone,nr)					// update real value
gUpd{|Real|} (UpdSearch val cnt)     	r = (UpdSearch val (dec cnt),r)		// continue search, don't change
gUpd{|Real|} (UpdCreate l)			 	_ = (UpdCreate l,0.0)				// create default value
gUpd{|Real|} mode 			  	     	r = (mode,r)						// don't change

gUpd{|Bool|} (UpdSearch (UpdB nb) 0) 	_ = (UpdDone,nb)					// update boolean value
gUpd{|Bool|} (UpdSearch val cnt)     	b = (UpdSearch val (dec cnt),b)		// continue search, don't change
gUpd{|Bool|} (UpdCreate l)			 	_ = (UpdCreate l,False)				// create default value
gUpd{|Bool|} mode 			  	     	b = (mode,b)						// don't change

gUpd{|String|} (UpdSearch (UpdS ns) 0) 	_ = (UpdDone,ns)					// update string value
gUpd{|String|} (UpdSearch val cnt)     	s = (UpdSearch val (dec cnt),s)		// continue search, don't change
gUpd{|String|} (UpdCreate l)		   	_ = (UpdCreate l,"")				// create default value
gUpd{|String|} mode 			  	 	s = (mode,s)						// don't change

gUpd{|UNIT|} mode _			= (mode,UNIT)

gUpd{|PAIR|} gUpda gUpdb mode=:(UpdCreate l) _								// invent a pair
# (mode,a)					= gUpda mode (abort "PAIR a evaluated")
# (mode,b)					= gUpdb mode (abort "PAIR b evaluated")
= (mode,PAIR a b)
gUpd{|PAIR|} gUpda gUpdb mode (PAIR a b)									// pass mode to a and b in all other cases
# (mode,a)					= gUpda mode a
# (mode,b)					= gUpdb mode b
= (mode,PAIR a b)

gUpd{|EITHER|} gUpda gUpdb (UpdCreate [ConsLeft:cl]) _
# (mode,a)					= gUpda (UpdCreate cl) (abort "LEFT a evaluated")
= (mode,LEFT a)
gUpd{|EITHER|} gUpda gUpdb (UpdCreate [ConsRight:cl]) _
# (mode,b)					= gUpdb (UpdCreate cl) (abort "RIGHT b evaluated")
= (mode,RIGHT b)
gUpd{|EITHER|} gUpda gUpdb (UpdCreate []) _ 
# (mode,b)					= gUpdb (UpdCreate []) (abort "Empty EITHER evaluated")
= (mode,RIGHT b)
gUpd{|EITHER|} gUpda gUpdb mode (LEFT a)
# (mode,a)					= gUpda mode a
= (mode,LEFT a)
gUpd{|EITHER|} gUpda gUpdb mode (RIGHT b)
# (mode,b)					= gUpdb mode b
= (mode,RIGHT b)

gUpd{|OBJECT|} gUpdo (UpdCreate l) _ 										// invent new type
# (mode,o)					= gUpdo (UpdCreate l) (abort "OBJECT evaluated")
= (mode,OBJECT o)
gUpd{|OBJECT of typedes|} gUpdo (UpdSearch (UpdC cname) 0) (OBJECT o) 		// constructor changed of this type
# (mode,o)					= gUpdo (UpdCreate path) o
= (UpdDone,OBJECT o)
where
	path					= getConsPath (hd [cons \\ cons <- typedes.gtd_conses | cons.gcd_name == cname])
gUpd{|OBJECT|} gUpdo (UpdSearch val cnt) (OBJECT o)							// search further
# (mode,o)					= gUpdo (UpdSearch val (dec cnt)) o
= (mode,OBJECT o)
gUpd{|OBJECT|} gUpdo mode (OBJECT o)										// other cases
# (mode,o)					= gUpdo mode o
= (mode,OBJECT o)

gUpd{|CONS|} gUpdo (UpdCreate l) _ 											// invent new constructor ??
# (mode,c)					= gUpdo (UpdCreate l) (abort "CONS evaluated")
= (mode,CONS c)
gUpd{|CONS|} gUpdo mode (CONS c) 											// other cases
# (mode,c)					= gUpdo mode c
= (mode,CONS c)

gUpd{|FIELD|} gUpdx (UpdCreate l) _  										// invent new type 
# (mode,x)					= gUpdx (UpdCreate l) (abort "Value of FIELD evaluated")
= (mode,FIELD x)
gUpd{|FIELD|} gUpdx mode (FIELD x)											// other cases
# (mode,x)					= gUpdx mode x
= (mode,FIELD x)

gUpd{|(->)|} gUpdArg gUpdRes mode f
= (mode,f)	

// small utility functions

mkInput :: !Int !(InIDataId d) Value UpdValue !*HSt -> (BodyTag,*HSt) 
mkInput size (init,formid=:{mode}) val updval hst=:{cntr,submits} 
| mode == Edit || mode == Submit
	= ( Input 	[ Inp_Type		Inp_Text
				, Inp_Value		val
				, Inp_Name		(encodeTriplet (formid.id,cntr,updval))
				, Inp_Size		size
				, `Inp_Std		[EditBoxStyle, Std_Title (showType val)]
				, `Inp_Events	if (mode == Edit && not submits) (callClean OnChange formid.mode "") []
				] ""
	  , setCntr (cntr+1) hst)
| mode == Display
	= ( Input 	[ Inp_Type		Inp_Text
				, Inp_Value		val
				, Inp_ReadOnly	ReadOnly
				, `Inp_Std		[DisplayBoxStyle]
				, Inp_Size		size
				] ""
		,setCntr (cntr+1) hst)
= ( EmptyBody,setCntr (cntr+1) hst)
where
	showType (SV  str) 	= "::String"
	showType (NQV str)	= "::String"
	showType (IV i)		= "::Int"
	showType (RV r) 	= "::Real"
	showType (BV b) 	= "::Bool"
		
toHtml :: a -> BodyTag | gForm {|*|} a
toHtml a
# (na,_)						= mkForm (Set,mkFormId "__toHtml" a <@ Display) (mkHSt emptyFormStates undef)
= BodyTag na.form

toHtmlForm :: !(*HSt -> *(Form a,*HSt)) -> [BodyTag] | gForm{|*|}, gUpd{|*|}, gPrint{|*|}, gParse{|*|}, TC a
toHtmlForm anyform 
# (na,hst)						= anyform (mkHSt emptyFormStates undef)
= na.form

toBody :: (Form a) -> BodyTag
toBody form						= BodyTag form.form

derive gUpd 	Inline
derive gParse 	Inline
derive gPrint 	Inline
derive gerda 	Inline
derive read 	Inline
derive write 	Inline

gForm{|Inline|} (init,formid) hst
# (Inline string) =  formid.ival 	
= ({changed=False, value=formid.ival, form=[InlineCode string]},incrHSt 2 hst)

showHtml :: [BodyTag] -> Inline
showHtml bodytags = Inline (foldl (+++) "" (reverse [x \\ x <|- gHpr {|*|} [|] bodytags]))

createDefault :: a | gUpd{|*|} a 
createDefault					= fromJust (snd (gUpd {|*|} (UpdSearch (UpdC "Just") 0) Nothing))
derive gUpd Maybe

setCntr :: InputId *HSt -> *HSt
setCntr i hst					= {hst & cntr = i}

incrHSt :: Int !*HSt -> *HSt
incrHSt i hst					= {hst & cntr = hst.cntr + i} // BUG ??????

CntrHSt :: !*HSt -> (Int,*HSt)
CntrHSt hst=:{cntr}				= (cntr,hst)

getChangedId :: !*HSt -> ([String],!*HSt)	// id of form that has been changed by user
getChangedId hst=:{states}
# (ids,states)					= getUpdateId states
= (ids,{hst & states = states })

// Enabling file IO on HSt

instance FileSystem HSt where
	fopen string int hst=:{world}
		# (bool,file,world)		= fopen string int world
		= (bool,file,{hst & world = world})

	fclose file hst=:{world}
		# (bool,world)			= fclose file world
		= (bool,{hst & world = world})

	stdio hst=:{world}
		# (file,world)			= stdio world
		= (file,{hst & world = world})

	sfopen string int hst=:{world}
		# (bool,file,world)		= sfopen string int world
		= (bool,file,{hst & world = world})

// General access to the World environment on HSt:

appWorldHSt :: !.(*World -> *World) !*HSt -> *HSt
appWorldHSt f hst=:{world}
	= {hst & world=appWorldNWorld f world}

accWorldHSt :: !.(*World -> *(.a,*World)) !*HSt -> (.a,!*HSt)
accWorldHSt f hst=:{world}
	# (a,world)	= accWorldNWorld f world
	= (a,{hst & world=world})

// test interface

runUserApplication :: .(*HSt -> *(.a,*HSt)) *FormStates *NWorld -> *(.a,*FormStates,*NWorld)
runUserApplication userpage states nworld
# (html,{states,world})			= userpage (mkHSt states nworld)
= (html,states,world)
