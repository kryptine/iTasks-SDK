implementation module WorkListHandler

import StdEnv
import Http
import Text
import JSON
import iDataHandler


:: WorkListItem = { taskid	:: String //Task id of the work item
				  , for		:: String //Label of the user who issued the work
				  , subject	:: String //Short description of the work
				  }

derive JSONEncode WorkListItem

handleWorkListRequest :: !(Task a) !HTTPRequest *World -> (!HTTPResponse, !*World)
handleWorkListRequest userpage req world = ({http_emptyResponse & rsp_data = toJSON worklist},world)
where
	worklist :: [WorkListItem]
	worklist = [{taskid = toString id, for = "Boss", subject = "Task with id " +++ toString id } \\ id <- [1 .. 5]]
	
	
//handleTaskCalculationRequest :: !HTTPRequest !UserId !(Task a) !*World -> (!Bool,!HtmlTree,!Maybe String,!*HtmlStream,!*World)


import InternaliTasksThreadHandling, StdStrictLists

handleWorkListRequest2 :: !(Task a) !HTTPRequest *World -> (!HTTPResponse, !*World) | iData a
handleWorkListRequest2 mainTask request world
# (toServer,htmlTree,maybeError,world)	= handleTaskCalculationRequest request 0 mainTask world
= ({http_emptyResponse & rsp_data = (toString ""/*html*/)}, world)

calculateTaskTree :: !UserId !(Task a) !*HSt  -> (!Bool,!HtmlTree,!Maybe String,!*HSt) | iData a
calculateTaskTree thisUser mainTask hst
# (pversion,hst)	 	= setPUserNr thisUser id hst												// fetch global settings of this user
# (sversion,hst)	 	= setSVersionNr thisUser id hst												// fetch version number of session (not needed in new set up?)
# versionconflict		= sversion > 0 && sversion < pversion.versionNr //&& not noNewVersion 		// test if there is a version conflict				
| versionconflict		= (True,BT [],Just "Version conflict detected!",hst)						// Yes, return error message

# ((toServer,thrOwner,event,thrinfo,threads),tst=:{html,hst,trace,activated})	
						=  calculateTasks thisUser pversion False mainTask (initTst thisUser TxtFile TxtFile hst)

# newUserVersionNr		= 1 + if (pversion.versionNr > sversion) pversion.versionNr sversion		// increment user querie version number
# (_,hst)				= clearIncPUser thisUser (\_ -> newUserVersionNr) hst						// store in session
# (sversion,hst)	 	= setSVersionNr thisUser (\_ -> newUserVersionNr) hst						// store in persistent memory
# showCompletePage		= IF_Ajax (hd threads == [-1]) True

= (toServer,html,Nothing,hst)
where
	initTst :: !UserId !Lifespan !Lifespan !*HSt -> *TSt
	initTst thisUser itaskstorage threadstorage hst
	=	{ tasknr		= [-1]
		, activated 	= True
		, staticInfo	= initStaticInfo thisUser threadstorage
		, userId		= if (thisUser >= 0) defaultUser thisUser
		, workflowLink	= (0,(defaultUser,0,defaultWorkflowName))
		, html 			= BT []
		, trace			= Nothing
		, hst 			= hst
		, options 		= initialOptions thisUser itaskstorage
		}

	initStaticInfo :: UserId !Lifespan -> StaticInfo
	initStaticInfo thisUser location
	=	{ currentUserId	= thisUser 
		, threadTableLoc= location
		}

	initialOptions ::  !UserId !Lifespan  -> !Options 
	initialOptions thisUser location 
	=	{ tasklife 		= if (thisUser >= 0) location Session 
		, taskstorage 	= PlainString
		, taskmode 		= Edit 
		, gc			= Collect
		}

handleTaskCalculationRequest :: !HTTPRequest !UserId !(Task a) !*World -> (!Bool,!HtmlTree,!Maybe String,!*World) | iData a
handleTaskCalculationRequest request thisUser mainTask world
# (gerda,world)				= openDatabase ODCBDataBaseName world						// open the relational database if option chosen
# (datafile,world)			= openmDataFile DataFileName world							// open the datafile if option chosen
# nworld 					= {worldC = world, inout = [|], gerda = gerda, datafile = datafile}	
# (initforms,nworld)	 	= retrieveFormStates request.arg_post nworld				// Retrieve the state information stored in an html page, other state information is collected lazily
# hst						= {(mkHSt initforms nworld) & request = request}			// Create the HSt
# (toServer, htmlTree, maybeError, {states,world}) 
							= calculateTaskTree thisUser mainTask hst					// Callculate the TaskTree given the id of the current user
# (debugOutput,states)		= if TraceOutput (traceStates states) (EmptyBody,states)	// Optional show debug information
# (pagestate, focus, world=:{worldC,gerda,inout,datafile})	
							= storeFormStates "" states world							// Store all state information
# worldC					= closeDatabase gerda worldC								// close the relational database if option chosen
# worldC					= closemDataFile datafile worldC							// close the datafile if option chosen
/*# inout						= IF_Ajax
								(print_to_stdout "" inout <+
								(pagestate) <+ State_FormList_Separator <+				// state information
        		 				AjaxCombine bodytags [debugInput,debugOutput]			// page, or part of a page
								)
								(print_to_stdout 										// Print out all html code
									(Html (Head headattr [mkJsTag, mkCssTag : headtags]) 
									(Body bodyattr [mkInfoDiv pagestate focus : bodytags ++ [debugInput,debugOutput]]))
									inout
								)
*/
= (toServer,htmlTree,maybeError,worldC)
where
	AjaxCombine [Ajax bodytags:ys] [EmptyBody,EmptyBody] 	= [Ajax bodytags:ys]
	AjaxCombine [Ajax bodytags:ys] debug 					= [Ajax [("debug",debug):bodytags]:ys]
	AjaxCombine [] debug 									= abort "AjaxCombine cannot combine empty result"
	
	debugInput				= if TraceInput (traceHtmlInput request.arg_post) EmptyBody


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

mkHSt :: *FormStates *NWorld -> *HSt
mkHSt states nworld = {cntr=0, states=states, request= http_emptyRequest, world=nworld, submits = False, issub = False }

mkCssTag :: HeadTag
mkCssTag = Hd_Link [Lka_Type "text/css", Lka_Rel Docr_Stylesheet, Lka_Href ExternalCleanStyles]

mkJsTag :: HeadTag
mkJsTag = Hd_Script [Scr_Src (ThisExe +++ "/js/clean.js"), Scr_Type TypeJavascript ] (SScript "")

mkInfoDiv :: String String -> BodyTag
mkInfoDiv state focus =
	Div [`Div_Std [Std_Style "display:none"]] [
	Div [`Div_Std [Std_Id "GS"]] [Txt state],
	Div [`Div_Std [Std_Id "FS"]] [Txt focus],
	Div [`Div_Std [Std_Id "AN"]] [Txt ThisExe],
	Div [`Div_Std [Std_Id "OPT-ajax"]] [Txt (IF_Ajax "true" "false")]
	]
