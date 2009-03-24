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

:: InputId	 	:== Int					// unique id for every constructor and basic value appearing in the state
:: FormUpdate	:== (InputId,UpdValue)	// info obtained when form is updated

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

// test interface

runUserApplication :: .(*HSt -> *(.a,*HSt)) HTTPRequest *FormStates *NWorld -> *(.a,*FormStates,*NWorld)
runUserApplication userpage request states nworld
# (html,{states,world})			= userpage (mkHSt request states nworld)
= (html,states,world)
