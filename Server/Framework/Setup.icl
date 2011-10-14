implementation module Setup

import StdList,StdBool, StdInt, StdFile, StdFunc
import HTTP, Map
import HTML, HtmlUtil
import File, Error, OS
import Config
import Engine, Util

setupHandler :: !HTTPRequest !*World -> (!HTTPResponse, !*World)
setupHandler req world	
	# (appName,world)	= determineAppName world
	# (config,world)	= if (isEmpty (toList req.arg_post)) (initialConfig world) (postedConfig req, world)
	# (errors,world)	= checkConfig config world
	= case req.req_path of
		"/edit"					= editConfigPage appName config errors world
		"/root"
			| noErrors errors	= rootPasswordPage appName config world
			| otherwise			= editConfigPage appName config errors world
		"/save" 				= saveConfigPage appName config world
		_						= choicePage appName config errors world

//Initial config of the form
initialConfig :: !*World -> (!Config,!*World)
initialConfig world
	# (clientPath,world)	= findClientPath 10 "Client" world
	# (runAsyncPath,world) 	= findPath 10 ("Tools" </> "RunAsync" </> IF_POSIX_OR_WINDOWS "RunAsync" "RunAsync.exe") 
								defaultConfig.runAsyncPath world
	# (curlPath,world)		= findPath 10 ("Tools" </> "Curl" </> (IF_POSIX_OR_WINDOWS "curl" "curl.exe"))
								defaultConfig.curlPath world
	= ({defaultConfig & clientPath = clientPath, runAsyncPath = runAsyncPath, curlPath = curlPath},world)
where
	findClientPath 0 path world = (".",world)
	findClientPath i path world
		# (ok,world)	= checkClientPath path world
		| ok			= (path,world)
		//# buildpath		= path +++ "\\build"
		//# (ok,world)	= checkClientPath buildpath world
		//| ok			= (buildpath,world)
		= findClientPath (dec i) ("..\\" +++ path) world
		
	findPath 0 path defaultPath world = (defaultPath, world)
	findPath i path defaultPath world
		# (ok,world)	= fileExists path world
		| ok			= (path,world)
		= findPath (dec i) (".." </> path) defaultPath world
		
postedConfig :: !HTTPRequest -> Config	
postedConfig req = 
	{ clientPath = fromMaybe "" (get "clientPath" req.arg_post)
	, staticPath = fromMaybe "" (get "staticPath" req.arg_post)
	, rootPassword = fromMaybe "" (get "rootPassword" req.arg_post)
	, rootEmail	= fromMaybe "" (get "rootEmail" req.arg_post)
	, sessionTime = maybe 0 toInt (get "sessionTime" req.arg_post)
	, serverPort = maybe 0 toInt (get "serverPort" req.arg_post)
	, serverPath = fromMaybe "" (get "serverPath" req.arg_post)
	, debug = (fromMaybe "false" (get "debug" req.arg_post)) <> "false"
	, smtpServer = fromMaybe "" (get "smtpServer" req.arg_post)
	, generalWorkflows = (fromMaybe "false" (get "generalWorkflows" req.arg_post)) <> "false"
	, runAsyncPath = fromMaybe "" (get "runAsyncPath" req.arg_post)
	, curlPath = fromMaybe "" (get "curlPath" req.arg_post)
	}

checkConfig :: !Config !*World -> (![Maybe String],!*World)
checkConfig config world
	# (clientPathOk,world) = checkClientPath config.clientPath world
	# (runAsyncOk,world) = fileExists config.runAsyncPath world
	# (curlOk,world) = fileExists config.curlPath world
	= 	([if clientPathOk Nothing (Just CLIENT_ERROR)
		 ,Nothing
		 ,if (config.sessionTime < 60) (Just "Session time should be at least 60 seconds") Nothing
		 ,if ((config.serverPort < 0) || (config.serverPort > 60000)) (Just "Server port should be between 1 and 60000") Nothing
		 ,Nothing
		 ,Nothing
		 ,Nothing
		 ,Nothing
		 ,if runAsyncOk Nothing (Just RUNASYNC_ERROR)
		 ,if curlOk Nothing (Just CURL_ERROR)
		 ],world)

CLIENT_ERROR :== "The client framework could not be found at this location.<br />"
			 +++ "Please fill in the full path where the client framework can be found.<br />"
			 +++ "It can normally be found in the \"Client\\build\" folder of the SDK. For example C:\\iTasks-SDK\\Client\\build."

RUNASYNC_ERROR :== "The RunAsync tool could not be found at this location.<br />"
			   +++ "Please fill in the full path where the RunAsync tool can be found.<br />"
			   +++ "It can be compiled from the module RunAsync.icl, which can be found in the \"Tools\\RunAsync\" folder of the SDK. "

CURL_ERROR :== "The Curl tool could not be found at this location.<br />"
			   +++ "Please fill in the full path where the Curl tool can be found.<br />"

checkClientPath :: !String !*World -> (!Bool,!*World)
checkClientPath clientPath world
	# (res,world) = readFile (clientPath +++ "\\index.html") world
	= (isOk res,world)
	
configFileAvailable :: !String !*World -> (!Bool,!*World)
configFileAvailable appName world
	# (res,world) = readFile (appName +++ "-config.json") world
	= (isOk res, world)

noErrors :: [(Maybe String)] -> Bool
noErrors errors = not (or (map isJust errors))
	
page :: !String ![HtmlTag] !*World -> (!HTTPResponse, !*World)
page appName content world = ({newHTTPResponse & rsp_data = toString (pageLayout (appName +++ " setup") "" content)}, world)

choicePage :: !String !Config ![Maybe String] !*World -> (!HTTPResponse,!*World)
choicePage appName config errors world = page appName [DivTag [IdAttr "content"] [instructions,showConfig config errors],buttons] world
where
	instructions
		= PTag []
		[Text "Welcome, you are running ",StrongTag [] [Text appName],Text " for the first time.", BrTag[]
		,Text "You may run this application with the following default configuration, or edit it first."
		]
	buttons = DivTag [ClassAttr "buttons"]
			  [ButtonTag [TypeAttr "submit",OnclickAttr "window.location = '/root';"] [Text "Use this default configuration"]
			  ,ButtonTag [TypeAttr "submit",OnclickAttr "window.location = '/edit';"] [Text "Edit the configuration first"]
			  ]
		
editConfigPage :: !String !Config ![Maybe String] !*World -> (!HTTPResponse,!*World)
editConfigPage appName config errors world = page appName [form] world
where
	form = FormTag [MethodAttr "post",ActionAttr "/root"] [DivTag [IdAttr "content"] (editConfig config errors),submit]
	submit = DivTag [ClassAttr "buttons"] [ButtonTag [TypeAttr "submit"] [Text "Save configuration"]]

	instructions
		= PTag [] [Text "Please confirm the configuration settings below and save them."]
		
rootPasswordPage :: !String !Config !*World -> (!HTTPResponse,!*World)
rootPasswordPage appName config world = page appName [form] world
where
	form = FormTag [MethodAttr "post",ActionAttr "/save"] [DivTag [IdAttr "content"] (editRoot config),submit]
	submit = DivTag [ClassAttr "buttons"] [ButtonTag [TypeAttr "submit"] [Text "Save configuration"]]

	instructions
		= PTag [] [Text "Please confirm the root e-mail address and password."]

saveConfigPage :: !String !Config !*World -> (!HTTPResponse,!*World)
saveConfigPage appName config world
	# world = storeConfig appName config world
	= ({newHTTPResponse & rsp_headers = fromList [("X-Server-Control","stop")], rsp_data = toString (pageLayout (appName +++ " setup") "" content)}, world)
where
	content =	[ DivTag [IdAttr "content"]
					[ Text "The configuration file has been written.", BrTag []
					, Text "You can now run ", StrongTag [] [Text appName], Text ".", BrTag []
					]
				, DivTag [ClassAttr "buttons"] [ButtonTag [TypeAttr "submit",OnclickAttr ("window.location = '" +++ redirectUrl +++ "';")] [Text ("Run " +++ appName)]]
				]
	redirectUrl	= if (config.serverPort == 80) "http://localhost/" ("http://localhost:" +++ toString config.serverPort +++ "/")
			  
showConfig :: Config [Maybe String] -> HtmlTag
showConfig config errors = TableTag []
	[TrTag [ClassAttr (errclass error)] [ThTag [] [Text label,Text":"],TdTag [] [Text setting],TdTag [] (errmsg error) ] \\ (label,setting) <- fields & error <- errors]
where
	fields = [("Client path", config.clientPath)
			 ,("Static path", config.staticPath)
			 ,("Session time", toString config.sessionTime)
			 ,("Server port", toString config.serverPort)
			 ,("Server path", config.serverPath)
			 ,("Debug", toString config.debug)
			 ,("Smtp server", config.smtpServer)
			 ,("Enable general workflows", toString config.generalWorkflows)
			 ,("RunAsync path", config.runAsyncPath)
			 ,("Curl path", config.curlPath)
			 ]
editConfig :: !Config ![Maybe String] -> [HtmlTag]
editConfig config errors = [TableTag []
	[TrTag [ClassAttr (errclass error)] [ThTag [] [Text label,Text":"],TdTag [] [input],TdTag [] (errmsg error)] \\ (label,input) <- fields & error <- errors]
	: hidden]
where
	fields =	[("Client path",InputTag [TypeAttr "text",NameAttr "clientPath", ValueAttr config.clientPath])
				,("Static path",InputTag [TypeAttr "text",NameAttr "staticPath", ValueAttr config.staticPath])
				,("Session time",InputTag [TypeAttr "text",NameAttr "sessionTime",SizeAttr "2", ValueAttr (toString config.sessionTime)])
				,("Server port",InputTag [TypeAttr "text",NameAttr "serverPort",SizeAttr "2", ValueAttr (toString config.serverPort)])
				,("Server path",InputTag [TypeAttr "text",NameAttr "serverPath", ValueAttr config.serverPath])
				,("Debug",InputTag [TypeAttr "checkbox",NameAttr "debug":if config.debug [CheckedAttr] [] ])
				,("Smtp server",InputTag [TypeAttr "text",NameAttr "smtpServer", ValueAttr config.smtpServer])
				,("Enable general workflows",InputTag [TypeAttr "checkbox",NameAttr "generalWorkflows":if config.generalWorkflows [CheckedAttr] [] ])
				,("RunAsync path",InputTag [TypeAttr "text",NameAttr "runAsyncPath", ValueAttr config.runAsyncPath])
				,("Curl path",InputTag [TypeAttr "text",NameAttr "curlPath", ValueAttr config.curlPath])
				]
	hidden =	[ InputTag [TypeAttr "hidden",NameAttr "rootPassword", ValueAttr config.rootPassword]
				, InputTag [TypeAttr "hidden",NameAttr "rootEmail", ValueAttr config.rootEmail]
				]
			 
editRoot :: !Config -> [HtmlTag]
editRoot config = [TableTag []
	[TrTag [ClassAttr (errclass Nothing)] [ThTag [] [Text label,Text":"],TdTag [] [input]] \\ (label,input) <- fields]
	: hidden]
where
	fields =	[ ("Root password",InputTag [TypeAttr "text",NameAttr "rootPassword", ValueAttr config.rootPassword])
				, ("Root e-mail",InputTag [TypeAttr "text",NameAttr "rootEmail", ValueAttr config.rootEmail])
				]
	hidden =	[ InputTag [TypeAttr "hidden",NameAttr "clientPath", ValueAttr config.clientPath]
				, InputTag [TypeAttr "hidden",NameAttr "staticPath", ValueAttr config.staticPath]
				, InputTag [TypeAttr "hidden",NameAttr "sessionTime",ValueAttr (toString config.sessionTime)]
				, InputTag [TypeAttr "hidden",NameAttr "serverPort",ValueAttr (toString config.serverPort)]
				, InputTag [TypeAttr "hidden",NameAttr "serverPath", ValueAttr config.serverPath]
				, InputTag [TypeAttr "hidden",NameAttr "debug", ValueAttr (if config.debug "true" "false")]
				, InputTag [TypeAttr "hidden",NameAttr "smtpServer", ValueAttr config.smtpServer]
				, InputTag [TypeAttr "hidden",NameAttr "generalWorkflows", ValueAttr (if config.generalWorkflows "true" "false")]
				, InputTag [TypeAttr "hidden",NameAttr "runAsyncPath", ValueAttr config.runAsyncPath]
				, InputTag [TypeAttr "hidden",NameAttr "curlPath", ValueAttr config.curlPath]
				]

errclass error = if (isNothing error) "field-ok" "field-error"
errmsg Nothing = []
errmsg (Just msg) = [EmTag [] [RawText msg]]
