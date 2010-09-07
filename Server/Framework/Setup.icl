implementation module Setup

import StdList,StdBool, StdInt
import Http, HttpServer
import Html, HtmlUtil
import Config
import Engine, Util


setupHandler :: !(Config -> [(String -> Bool, (HTTPRequest *World -> *(!HTTPResponse,!HTTPServerControl,!*World)))]) !HTTPRequest !*World -> (!HTTPResponse, !HTTPServerControl, !*World)
setupHandler handlers req world	
	# (appName,world)	= determineAppName world
	# (finished,world)	= configFileAvailable appName world
	| finished
		= finishPage appName world
	# (config,world)	= if (isEmpty req.arg_post) (initialConfig world) (postedConfig req, world)
	# (errors,world)	= checkConfig config world
	= case req.req_path of
		"/edit"	= editConfigPage appName config errors world
		"/save"
			| noErrors errors	= saveConfigPage appName config (handlers config) world
			| otherwise			= editConfigPage appName config errors world
		_		= choicePage appName config errors world

//Initial config of the form
initialConfig :: !*World -> (!Config,!*World)
initialConfig world
	# (clientPath,world) = findClientPath 10 "Client" world
	= ({defaultConfig & clientPath = clientPath},world)
where
	findClientPath 0 path world = (".",world)
	findClientPath i path world
		# (ok,world)	= checkClientPath path world
		| ok			= (path,world)
		# buildpath		= path +++ "\\build"
		# (ok,world)	= checkClientPath buildpath world
		| ok			= (buildpath,world)
		= findClientPath (dec i) ("..\\" +++ path) world

postedConfig :: !HTTPRequest -> Config	
postedConfig req = 
	{ clientPath = http_getValue "clientPath" req.arg_post ""
	, staticPath = http_getValue "staticPath" req.arg_post ""
	, rootPassword = http_getValue "rootPassword" req.arg_post ""
	, sessionTime = toInt (http_getValue "sessionTime" req.arg_post "0")
	, serverPort = toInt (http_getValue "serverPort" req.arg_post "0")
	, serverPath = http_getValue "serverPath" req.arg_post "0"
	, debug = http_getValue "debug" req.arg_post "false" <> "false"
	, smtpServer = http_getValue "smtpServer" req.arg_post ""
	, generalWorkflows = http_getValue "generalWorkflows" req.arg_post "false" <> "false"
	} 
			 		 
checkConfig :: !Config !*World -> (![Maybe String],!*World)
checkConfig config world
	# (clientPathOk,world) = checkClientPath config.clientPath world
	= 	([if clientPathOk Nothing (Just CLIENT_ERROR)
		 ,Nothing
		 ,Nothing
		 ,if (config.sessionTime < 60) (Just "Session time should be at least 60 seconds") Nothing
		 ,if ((config.serverPort < 0) || (config.serverPort > 60000)) (Just "Server port should be between 1 and 60000") Nothing
		 ,Nothing
		 ,Nothing
		 ,Nothing
		 ,Nothing
		 ],world)

CLIENT_ERROR :== "The client framework could not be found at this location.<br />"
			 +++ "Please fill in the full path where the client framework can be found.<br />"
			 +++ "It can normally be found in the \"Client\\build\" folder of the SDK. For example C:\\iTasks-SDK\\Client\\build."

checkClientPath :: !String !*World -> (!Bool,!*World)
checkClientPath clientPath world
	# (index,world) = readfile (clientPath +++ "\\index.html") world
	= (index <> "",world)

configFileAvailable :: !String !*World -> (!Bool,!*World)
configFileAvailable appName world
	# (config,world) = readfile (appName +++ "-config.json") world
	= (config <> "",world)

noErrors :: [(Maybe String)] -> Bool
noErrors errors = not (or (map isJust errors))
	
page :: !String ![HtmlTag] !*World -> (!HTTPResponse,!HTTPServerControl, !*World)
page appName content world = ({http_emptyResponse & rsp_data = toString (pageLayout (appName +++ " setup") "" content)}, HTTPServerContinue, world)

choicePage :: !String !Config ![Maybe String] !*World -> (!HTTPResponse,!HTTPServerControl,!*World)
choicePage appName config errors world = page appName [DivTag [IdAttr "content"] [instructions,showConfig config errors],buttons] world
where
	instructions
		= PTag []
		[Text "Welcome, you are running ",StrongTag [] [Text appName],Text " for the first time.", BrTag[]
		,Text "You may run this application with the following default configuration, or edit it first"
		]
	buttons = DivTag [ClassAttr "buttons"]
			  [ButtonTag [TypeAttr "submit",OnclickAttr "window.location = '/save';"] [Text "Use this default configuration"]
			  ,ButtonTag [TypeAttr "submit",OnclickAttr "window.location = '/edit';"] [Text "Edit the configuration first"]
			  ]
		
editConfigPage :: !String !Config ![Maybe String] !*World -> (!HTTPResponse,!HTTPServerControl,!*World)
editConfigPage appName config errors world = page appName [form] world
where
	form = FormTag [MethodAttr "post",ActionAttr "/save"] [DivTag [IdAttr "content"] [editConfig config errors],submit]
	submit = DivTag [ClassAttr "buttons"] [ButtonTag [TypeAttr "submit"] [Text "Save configuration and restart"]]

	instructions
		= PTag [] [Text "Please confirm the configuration settings below and save them."]

saveConfigPage :: !String !Config ![(String -> Bool, (HTTPRequest *World -> *(!HTTPResponse,!HTTPServerControl,!*World)))] !*World -> (!HTTPResponse,!HTTPServerControl,!*World)
saveConfigPage appName config handlers world
	# world 		= storeConfig appName config world
	# options 		= [HTTPServerOptPort config.serverPort, HTTPServerOptDebug config.debug]
	# redirectUrl	= if (config.serverPort == 80) "http://localhost/" ("http://localhost:" +++ toString config.serverPort +++ "/")
	= ({http_emptyResponse & rsp_headers = [("Status","302"),("Location",redirectUrl)]}, HTTPServerRestart options handlers, world)

finishPage :: !String !*World -> (!HTTPResponse, !HTTPServerControl, !*World)
finishPage appName world = page appName [instructions] world
where
	instructions
		= DivTag [IdAttr "content"] [Text "The configuration file has been written.",BrTag []
				  ,Text "Please restart the server to start ",StrongTag [] [Text appName]]	
			  
showConfig :: Config [Maybe String] -> HtmlTag
showConfig config errors = TableTag []
	[TrTag [ClassAttr (errclass error)] [ThTag [] [Text label,Text":"],TdTag [] [Text setting],TdTag [] (errmsg error) ] \\ (label,setting) <- fields & error <- errors]
where
	fields = [("Client path", config.clientPath)
			 ,("Static path", config.staticPath)
			 ,("Root password", config.rootPassword)
			 ,("Session time", toString config.sessionTime)
			 ,("Server port", toString config.serverPort)
			 ,("Server path", config.serverPath)
			 ,("Debug", toString config.debug)
			 ,("Smtp server", config.smtpServer)
			 ,("Enable general workflows", toString config.generalWorkflows)
			 ]
editConfig :: !Config ![Maybe String] -> HtmlTag
editConfig config errors = TableTag []
	[TrTag [ClassAttr (errclass error)] [ThTag [] [Text label,Text":"],TdTag [] [input],TdTag [] (errmsg error)] \\ (label,input) <- fields & error <- errors]
where
	fields = [("Client path",InputTag [TypeAttr "text",NameAttr "clientPath", ValueAttr config.clientPath])
			 ,("Static path",InputTag [TypeAttr "text",NameAttr "staticPath", ValueAttr config.staticPath])
			 ,("Root password",InputTag [TypeAttr "text",NameAttr "rootPassword", ValueAttr config.rootPassword])
			 ,("Session time",InputTag [TypeAttr "text",NameAttr "sessionTime",SizeAttr "2", ValueAttr (toString config.sessionTime)])
			 ,("Server port",InputTag [TypeAttr "text",NameAttr "serverPort",SizeAttr "2", ValueAttr (toString config.serverPort)])
			 ,("Server path",InputTag [TypeAttr "text",NameAttr "serverPath", ValueAttr config.serverPath])
			 ,("Debug",InputTag [TypeAttr "checkbox",NameAttr "debug":if config.debug [CheckedAttr] [] ])
			 ,("Smtp server",InputTag [TypeAttr "text",NameAttr "smtpServer", ValueAttr config.smtpServer])
			 ,("Enable general workflows",InputTag [TypeAttr "checkbox",NameAttr "generalWorkflows":if config.generalWorkflows [CheckedAttr] [] ])
			 ]

errclass error = if (isNothing error) "field-ok" "field-error"
errmsg Nothing = []
errmsg (Just msg) = [EmTag [] [RawText msg]]

