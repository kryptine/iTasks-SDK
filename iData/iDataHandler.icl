implementation module iDataHandler

defsize :== 100

import StdArray, StdChar, StdList, StdStrictLists, StdString, StdTuple, StdFile
import ArgEnv, StdMaybe
import iDataTrivial, iDataSettings, iDataState
import StdGeneric, GenParse, GenPrint
import Http, HttpUtil, HttpServer, HttpTextUtil
import Gerda
import StdBimap
import HSt
import Html

derive gPrint (,), (,,), (,,,), UpdValue
derive gParse (,), (,,), (,,,), UpdValue
derive gUpd		   (,,), (,,,)
derive bimap Form, FormId

gParse{|(->)|} gArg gRes _ 		= Nothing 
gPrint{|(->)|} gArg gRes _ _	= abort "functions can only be used with dynamic storage option!\n" 


:: InputId	 	:== Int							// unique id for every constructor and basic value appearing in the state
:: FormUpdate	:== (InputId,UpdValue)			// info obtained when form is updated

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


//doHtmlWrapper: When using a client/server architecture, the doHtmlWrapper may be used instead of
//doHtmlServer. It switches between doHtmlServer and doHtmlClient depending on which compile option
//is selected.

doHtmlWrapper :: !UserPage !*World -> *World
doHtmlWrapper userpage world = IF_Client (doHtmlClient userpage world) (doHtmlServer userpage world)

// doHtmlServer: top level function given to end user.
// It sets up the communication with a (sub)server or client, depending on the option chosen.

doHtmlServer :: !UserPage !*World -> *World
doHtmlServer userpage world
| ServerKind == Internal
	# world	= instructions world
	= StartServer userpage world									// link in the Clean http 1.0 server	
//| ServerKind == External											// connect with http 1.1 server
//| ServerKind == CGI												// build as CGI application
| otherwise
	= unimplemented world
where
	instructions :: *World -> *World
	instructions world
		# (console, world)	= stdio world
		# console			= fwrites "HTTP server started...\n" console
		# console			= fwrites ("Please point your browser to http://localhost/" +++ ThisExe +++ "\n") console
		# (_,world)			= fclose console world
		= world
		
	unimplemented :: *World -> *World
	unimplemented world
		# (console, world)	= stdio world
		# console			= fwrites "The chosen server mode is not supported.\n" console
		# console			= fwrites "Please select ServerKind Internal in iDataSettings.dcl.\n" console
		# (_,world)			= fclose console world
		= world

StartServer :: !UserPage !*World -> *World
StartServer userpage world
	# options = ServerOptions ++ (if TraceHTTP [HTTPServerOptDebug True] [])
	= http_startServer options   [((==) ("/" +++ ThisExe), IF_Ajax doAjaxInit (doDynamicResource userpage))
								 ,((==) ("/" +++ ThisExe +++ "_ajax"), IF_Ajax (doDynamicResource userpage) http_notfoundResponse)
								 ,(\_ -> True, doStaticResource)
								 ] world

// Request handler which serves static resources from the application directory,
// or a system wide default directory if it is not found locally.
// This request handler is used for serving system wide javascript, css, images, etc...
doStaticResource :: !HTTPRequest *World -> (!HTTPResponse, !*World)
doStaticResource req world
	# filename				= MyAbsDir +++ req.req_path	
	# (type, world)			= http_staticFileMimeType filename world
	# (ok, content, world)	= http_staticFileContent filename world
	| ok					= ({rsp_headers = [("Status","200 OK"),
											   ("Content-Type", type),
											   ("Content-Length", toString (size content))]
							   ,rsp_data = content}, world)
	# filename				= ResourceDir +++ (req.req_path % ((size ThisExe) + 1, size req.req_path)) //Remove the /(ThisExe)/ from the filename
	# (type, world)			= http_staticFileMimeType filename world
	# (ok, content, world)	= http_staticFileContent filename world
	|  ok 					= ({rsp_headers = [("Status","200 OK"),
											   ("Content-Type", type),
											   ("Content-Length", toString (size content))]
							   	,rsp_data = content}, world)		 							   
	= http_notfoundResponse req world

// Request handler which handles dynamically generated pages.
doDynamicResource :: !UserPage !HTTPRequest !*World -> (!HTTPResponse, !*World)
doDynamicResource userpage request world
	# (toServer,html,world)	= doHtmlPage request userpage world
	= ({http_emptyResponse & rsp_data = html}, world)

// General entry used by all servers and client to calculate the next page
doHtmlPage :: !HTTPRequest !.UserPage !*World -> (!Bool,!String,!*World)
doHtmlPage request userpage world
# (gerda,world)				= openDatabase ODCBDataBaseName world						// open the relational database if option chosen
# (datafile,world)			= openmDataFile DataFileName world							// open the datafile if option chosen
# nworld 					= {worldC = world, gerda = gerda, datafile = datafile}	
# (initforms,nworld)	 	= retrieveFormStates request.arg_post nworld				// Retrieve the state information stored in an html page, other state information is collected lazily
# hst						= (mkHSt request initforms nworld) 							// Create the HSt
# ((toServer,prefix),html /*HtmlTag attr [HeadTag headattr headtags, BodyTag bodyattr bodytags] */,{states,world}) 
							= userpage hst												// Call the user application
# (debugOutput,states)		= if TraceOutput (traceStates states) (SpanTag [] [],states)			// Optional show debug information
# (pagestate, focus, world=:{worldC,gerda, datafile})	
							= storeFormStates prefix states world						// Store all state information
# worldC					= closeDatabase gerda worldC								// close the relational database if option chosen
# worldC					= closemDataFile datafile worldC							// close the datafile if option chosen
/*
# html						= IF_Ajax
								(pagestate +++ State_FormList_Separator +++				// state information
        		 					toString (AjaxCombine bodytags debugInput debugOutput )	// page, or part of a page
								)
								(toString 										// Print out all html code
									(HtmlTag [] [HeadTag headattr [mkJsTag, mkCssTag : headtags],
												 BodyTag bodyattr [mkInfoDiv pagestate focus : bodytags ++ [debugInput,debugOutput]] ])
								)
*/
# html						= toString html
= (toServer,html,worldC)
where
	AjaxCombine l1 l2 l3 	= DivTag [] (l1 ++ l2 ++ l3)	
	debugInput				= if TraceInput (traceHtmlInput request.arg_post) []


mkPage :: [HtmlAttr] [HtmlTag] [HtmlAttr] [HtmlTag] -> HtmlTag
mkPage headattr headtags bodyattr bodytags = HtmlTag [] [HeadTag headattr headtags, BodyTag bodyattr bodytags]

mkCssTag :: HtmlTag
mkCssTag = LinkTag [TypeAttr "text/css", RelAttr "stylesheet", HrefAttr "css/clean.css"] []

mkJsTag :: HtmlTag
mkJsTag = ScriptTag [SrcAttr (ThisExe +++ "/js/clean.js"), TypeAttr "text/javascript"] []

mkInfoDiv :: String String -> HtmlTag
mkInfoDiv state focus =
	DivTag [StyleAttr "display: none"] [
		DivTag [IdAttr "GS"] [Text state],
		DivTag [IdAttr "FS"] [Text focus],
		DivTag [IdAttr "AN"] [Text ThisExe],
		DivTag [IdAttr "OPT-ajax"] [Text (IF_Ajax "true" "false")]
	]

doAjaxInit :: !HTTPRequest !*World -> (!HTTPResponse, !*World)
doAjaxInit req world = ({http_emptyResponse & rsp_data = toString page}, world)
where
	page = mkPage [] [mkCssTag, mkJsTag] [] [ mkInfoDiv "" ""]
		
		/*
		 , Div [`Div_Std [Std_Id "thePage", Std_Class "thread"]] [Txt (ThisExe +++ " loading. Please wait...")]
		 , Div [`Div_Std [Std_Id "iTaskInfo", Std_Class "thread"]] []
		 , Div [`Div_Std [Std_Id "debug", Std_Class "thread"]] []
		 ] ++ (IF_ClientServer 
		 		[Applet [`Apl_Std [Std_Id "SA", Std_Style "visibility: hidden; width: 1px; height: 1px;"]
		 				, Apl_Archive (ThisExe +++ "/jar/jme.jar")
		 				, Apl_Codebase "."
		 				, Apl_Code "jme.ClientApplet.class"	
		 				] ""
		 		]
		 		[]
		 	   )
		)
*/				  	
//This wrapper is used when the program is run on the client in the sapl interpreter
doHtmlClient :: UserPage !*World -> *World
doHtmlClient userpage world
# (sio,world) 					= stdio world
# (data,sio)					= freadline sio															//Fetch all input
# req							= http_emptyRequest
# (req,mdone,hdone,ddone,error)	= http_addRequestData req False False False data
# req							= http_parseArguments req												//Parse the parameters int he request
# (toServer,html,world)			= doHtmlPage req userpage world											//Run the user page
# sio							= fwrites (toString toServer +++ "#0#" +++ toString html) sio			//Write the output
# world							= snd (fclose sio world)
= world

// The function mkViewForm is *the* magic main function 
// All idata /itasks end up in this function
// It does everything, a swiss army knife editor that makes coffee too ...

mkViewForm :: !(InIDataId d) !(HBimap d v) !*HSt -> (Form d,!*HSt) | iData v
mkViewForm (init,formid) bm=:{toForm, updForm, fromForm, resetForm} hst=:{request,states,world,submits,issub} 
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
		  ,mkHSt request states world)

	# (viewform,{states,world})											// make a form for it
							= mkForm (init,if (init == Const) vformid (reuseFormId formid view)) ({mkHSt request states world & submits = submits, issub = issub})

	| viewform.changed && not isupdated						 			// important: redo it all to handle the case that a user defined specialisation is updated !!
							= calcnextView True (Just viewform.value) states world

	# (states,world)		= replaceState` vformid viewform.value states world	// store new value into the store of states

	= (	{ changed			= isupdated
		, value				= newval
		, form				= viewform.form
		}
	  ,{mkHSt request states world & issub = issub})

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

// It can be convenient to explicitly delete IData, in particular for persistent IData object
// or to optimize iTasks
// All IData objects administrated in the state satisfying the predicate will be deleted, no matter where they are stored.

deleteIData :: !String !*HSt -> *HSt
deleteIData prefix hst=:{states,world}
# (states,world) = deleteStates prefix states world
= {hst & states = states, world = world}

changeLifespanIData :: !String !Lifespan !Lifespan !*HSt -> *HSt
changeLifespanIData prefix oldspan newspan hst=:{states,world}
# (states,world) = changeLifetimeStates  prefix oldspan newspan states world
= {hst & states = states, world = world}


// specialize has to be used if a programmer wants to specialize gForm.
// It remembers the current value of the index in the expression and creates an editor to show this value.
// The value might have been changed with this editor, so the value returned might differ from the value you started with!

specialize :: !((InIDataId a) *HSt -> (Form a,*HSt)) !(InIDataId a) !*HSt -> (!Form a,!*HSt) | gUpd {|*|} a
specialize editor (init,formid) hst=:{cntr = inidx,states = formStates,submits,world}
# nextidx					= incrIndex inidx formid.ival		// this value will be repesented differently, so increment counter 
# (nv,hst) 					= editor (init,nformid) (setHStCntr 0 hst)
= (nv,{setHStCntr nextidx hst & submits = submits})
where
	nformid					= {formid & id = formid.id <+++ "_specialize_" <+++ inidx <+++ "_"}

	incrIndex :: Int v -> Int | gUpd {|*|} v
	incrIndex i v
	# (UpdSearch _ cnt,v)	= gUpd {|*|} (UpdSearch (UpdI 0) -1) v
	= i + (-1 - cnt)


// gForm: automatically derives a Html form for any Clean type

mkForm :: !(InIDataId a) *HSt -> *(Form a, !*HSt)	| gForm {|*|} a
mkForm (init,formid) hst =: {issub}
# (form,hst) 	= gForm{|*|} (init,formid) {hst & submits = (formid.mode == Submit), issub = True}	//Use gForm to create the html form
# buttons		= if (formid.mode == Submit) 										 				//Add submit and clear buttons to a form in submit mode.
						[ BrTag []
						, InputTag [TypeAttr "submit", ValueAttr "Submit"]
						, InputTag [TypeAttr "reset", ValueAttr "Clear"]
						, BrTag []
						]
						[ InputTag [TypeAttr "submit", StyleAttr "display: none"]]					//Add a hidden submit input to allow updates on press of "enter" button 

# sform			= if issub
					form.form
					[FormTag [ ActionAttr ("/" +++ ThisExe)											//Wrap the form in html form tags
						, MethodAttr "post"
						, NameAttr  (encodeString formid.id)										//Enable the use of any character in a form name
						, EnctypeAttr "multipart/form-data"
						, OnsubmitAttr "return catchSubmit(this);"
						, IdAttr (encodeString formid.id)
						] (form.form ++ buttons)
				  	] 
= ({form & form = sform} ,hst)

generic gForm a :: !(InIDataId a) !*HSt -> *(Form a, !*HSt)	

gForm{|Int|} (init,formid) hst 	
# (body,hst)				= mkInput defsize (init,formid) (toString i) (UpdI i) hst
= ({ changed				= False
   , value					= i
   , form					= [body]
   },hst)
where
	i						= formid.ival 
gForm{|Real|} (init,formid) hst 	
# (body,hst)				= mkInput defsize (init,formid) (toString r) (UpdR r) hst
= ({ changed				= False
   , value					= r
   , form					= [body]
   },hst)
where
	r						= formid.ival 

/*
gForm{|Bool|} (init,formid) hst // PK
# (body,hst)				= mkInput defsize (init,formid) (BV b) (UpdB b) hst
= ({ changed				= False
   , value					= b
   , form					= [body]
   },hst)
where
	b						= formid.ival 
*/
gForm{|Bool|} (init,formid) hst=:{cntr,submits} // PK
= ({ changed				= False
   , value					= formid.ival
   , form					= [mkConsSel cntr (toString formid.ival) ["False","True"] (if formid.ival 1 0) formid submits]
   },setHStCntr (cntr+1) hst)

gForm{|String|} (init,formid) hst 	
# (body,hst)				= mkInput defsize (init,formid) s (UpdS s) hst
= ({ changed				= False
   , value					= s
   , form					= [body]
   },hst)
where
	s						= formid.ival 
gForm{|UNIT|}  _ hst
= ({ changed				= False
   , value					= UNIT
   , form					= []
   },hst)
gForm{|PAIR|} gHa gHb (init,formid) hst 
# (na,hst)					= gHa (init,reuseFormId formid a) hst
# (nb,hst)					= gHb (init,reuseFormId formid b) hst
= ({ changed				= na.changed || nb.changed
   , value					= PAIR na.value nb.value
   , form					= [TableTag [StyleAttr "padding: 0px"] [TrTag [] [TdTag [] na.form, TdTag [] nb.form]]]
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
	# (nc,hst)				= gHc (init,reuseFormId formid c) (setHStCntr (cntr+1) hst) // don't display record constructor
	= ({nc & value=CONS nc.value},hst)
| t.gcd_type_def.gtd_num_conses == 1 
	# (nc,hst)				= gHc (init,reuseFormId formid c) (setHStCntr (cntr+1) hst) // don't display constructors that have no alternative
	= ({nc & value=CONS nc.value},hst)
| t.gcd_name.[(size t.gcd_name) - 1] == '_' 										 // don't display constructor names which end with an underscore
	# (nc,hst)				= gHc (init,reuseFormId formid c) (setHStCntr (cntr+1) hst) 
	= ({nc & value=CONS nc.value},hst)
# (selector,hst)			= mkConsSelector formid t hst
# (nc,hst)					= gHc (init,reuseFormId formid c) hst
= ({ changed				= nc.changed
   , value					= CONS nc.value
   , form					= [TableTag [StyleAttr "padding: 0px"] [selector : nc.form]]
   },hst)
where
	(CONS c)				= formid.ival

mkConsSelector formid thiscons hst=:{cntr,submits} // PK: lifted to a global function 
						= (mkConsSel cntr myname allnames myindex formid submits, setHStCntr (cntr+1) hst)
where
	myname				= thiscons.gcd_name
	allnames			= map (\n -> n.gcd_name) thiscons.gcd_type_def.gtd_conses
	myindex				= case allnames ?? myname of
							-1			-> abort ("cannot find index of " +++ myname )
							i			-> i

mkConsSel :: Int String [String] Int (FormId x) Bool -> HtmlTag  // PK: lifted to a global function 
mkConsSel cntr myname list nr formid submits
	= SelectTag [ NameAttr (selectorInpName +++ encodeString myname): styles ]		// changed to see changes in case of a submit
			 [ OptionTag
				[ValueAttr (encodeTriplet (formid.id,cntr,UpdC elem)) : if (j == nr) [SelectedAttr] [] ] [Text elem]
			 \\ elem <- list & j <- [0..]
			 ] 
	where
		styles			= case formid.mode of
							Edit	-> [ IdAttr (encodeInputId (formid.id,cntr,UpdC myname))] ++ (if submits [] (callClean "change" Edit "" formid.lifespan True))
							Submit	-> [ IdAttr (encodeInputId (formid.id,cntr,UpdC myname))]
							_		-> [ DisabledAttr]

gForm{|FIELD of d |} gHx (init,formid) hst 
# (nx,hst)					= gHx (init,reuseFormId formid x) hst
| d.gfd_name.[(size d.gfd_name) - 1] == '_' 										 // don't display field names which end with an underscore
							= ({ changed	= False
							   , value		= formid.ival
							   , form		= []
							   },hst)
= ({ changed				= nx.changed
   , value					= FIELD nx.value
   , form					= [TableTag [StyleAttr "padding: 0px"] [fieldname :  nx.form] ]
   },hst)
where
	(FIELD x)				= formid.ival
	
	fieldname				= InputTag	[ TypeAttr		"text"
										, ValueAttr		(prettify d.gfd_name +++ ": ")	
										, DisabledAttr
										]
	prettify name			= mkString [toUpper lname : addspace lnames]
	where
		[lname:lnames]		= mkList name
		addspace []			= []
		addspace [c:cs]
		| isUpper c			= [' ',toLower c:addspace cs]
		| otherwise			= [c:addspace cs]

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

//gUpd{|Bool|} (UpdSearch (UpdB nb) 0) 	_ = (UpdDone,nb)					// update boolean value
gUpd{|Bool|} (UpdSearch (UpdC nb) 0) 	_ = (UpdDone,nb=="True")					// update boolean value
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

mkInput :: !Int !(InIDataId d) String UpdValue !*HSt -> (HtmlTag,*HSt) 
mkInput size (init,formid=:{mode}) val updval hst=:{cntr,submits} 
| mode == Edit || mode == Submit
	= ( InputTag 	[ TypeAttr		"text"
					, ValueAttr		val
					, NameAttr		(encodeTriplet (formid.id,cntr,updval))
					, SizeAttr		(toString size)
					, IdAttr (encodeInputId (formid.id,cntr,updval))
					: if (mode == Edit && not submits) (callClean "change" formid.mode "" formid.lifespan False) []
					]
	  , setHStCntr (cntr+1) hst)
| mode == Display
	= ( InputTag 	[ TypeAttr		"text"
					, ValueAttr		val
					, SizeAttr		(toString size)
					]
		,setHStCntr (cntr+1) hst)
= ( SpanTag [] [],setHStCntr (cntr+1) hst)
		
// The following two functions are not an example of decent Clean programming, but it works thanks to lazy evaluation...

toHtml :: a -> HtmlTag | gForm {|*|} a
toHtml a
# (na,_)						= mkForm (Set,mkFormId "__toHtml" a <@ Display) (mkHSt http_emptyRequest emptyFormStates dummy)
= BodyTag [] na.form
where
	dummy = { worldC 	= abort "dummy world for toHtml!\n"
			, gerda	 	= abort "dummy gerda for toHtml!\n"
			, datafile	= abort "dummy datafile for toHtml!\n"
			}


toHtmlForm :: !(*HSt -> *(Form a,*HSt)) -> [HtmlTag] | gForm{|*|}, gUpd{|*|}, gPrint{|*|}, gParse{|*|}, TC a
toHtmlForm anyform 
//# (na,hst)					= anyform (mkHSt emptyFormStates (abort "illegal call to toHtmlForm!\n"))
# (na,hst)						= anyform (mkHSt http_emptyRequest emptyFormStates (abort "illegal call to toHtmlForm!\n"))
= na.form
where
	dummy = { worldC 	= abort "dummy world for toHtmlForm!\n"
			, gerda	 	= abort "dummy gerda for toHtmlForm!\n"
			, datafile	= abort "dummy datafile for toHtmlForm!\n"
			}

toBody :: (Form a) -> HtmlTag
toBody form						= BodyTag [] form.form

derive gUpd 	Inline
derive gParse 	Inline
derive gPrint 	Inline
derive gerda 	Inline
derive read 	Inline
derive write 	Inline

gForm{|Inline|} (init,formid) hst
# (Inline string) =  formid.ival 	
= ({changed=False, value=formid.ival, form=[RawText string]},incrHStCntr 2 hst)

showHtml :: [HtmlTag] -> Inline
showHtml tags = Inline (foldl (+++) "" (map toString tags))

createDefault :: a | gUpd{|*|} a 
createDefault					= fromJust (snd (gUpd {|*|} (UpdSearch (UpdC "Just") 0) Nothing))
derive gUpd Maybe

getChangedId :: !*HSt -> ([String],!*HSt)	// id of form that has been changed by user
getChangedId hst=:{states}
# (ids,states)					= getUpdateId states
= (ids,{hst & states = states })


// test interface

runUserApplication :: .(*HSt -> *(.a,*HSt)) HTTPRequest *FormStates *NWorld -> *(.a,*FormStates,*NWorld)
runUserApplication userpage request states nworld
# (html,{states,world})			= userpage (mkHSt request states nworld)
= (html,states,world)
