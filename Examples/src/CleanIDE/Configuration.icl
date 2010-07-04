implementation module Configuration

import iTasks, JSON, GUI, AppState

derive gPrint			IDEConfig
derive gParse			IDEConfig
derive gVisualize		IDEConfig
derive gUpdate			IDEConfig
derive gHint			IDEConfig
derive gError			IDEConfig
derive gMerge			IDEConfig
derive JSONEncode		IDEConfig
derive JSONDecode		IDEConfig
derive bimap			Maybe, (,)

loadConfig :: Task (Maybe IDEConfig)
loadConfig =
					accWorld readConfig
	>>= \mbConfig.	case mbConfig of
						Just config = return mbConfig
						Nothing =
											configWizard
							>>= \mbConfig.	case mbConfig of
												Just config =
														appWorld (writeConfig config)
													>>| return mbConfig
												Nothing = return Nothing

readConfig :: !*World -> (!Maybe IDEConfig, !*World)
readConfig world
	# (content,world) = readfile configFileName world
	= (fromJSON (fromString content),world)
	
writeConfig :: !IDEConfig !*World -> *World
writeConfig config world
	= writefile configFileName (toString (toJSON config)) world
	
configFileName = "cleanIDE-globalConfig.json"

configWizard = wizard "Configuration Wizard" steps initConfig
where
	steps =	[	ViewOnState	"Welcome to the Clean IDE configuration wizard!" []
			,	ViewOnState	"Choose the path where your projects are stored:"
							[	editor	{ editorFrom	= \config		-> config.projectsPath
										, editorTo		= \path config	-> {config & projectsPath = path}
										}
							]
			,	CustomTask	(\config prevAction -> case prevAction of
								GotoPrevious = return (config, GotoPrevious)
								GotoNext =
												isDirectory config.projectsPath
									>>= \ok.	case ok of
													True = return (config, GotoNext)
													False =
																		requestConfirmation ("Directory '" +++ config.projectsPath +++ "' does not exist. Should it be created?")
														>>= \create.	if create
																			(let
																				handleException = (\CannotCreate -> showMessageAbout "Error" ("Could not create '" +++ config.projectsPath +++ "'!") >>| return (config, GotoPrevious))
																			in
																				(try (createDirectory config.projectsPath >>| return (config, GotoNext)) handleException)
																			)
																			(return (config, GotoPrevious))
																	
							)
			,	ViewOnState	"Give the path to an old IDE executable:"
							[	editor	{ editorFrom	= \config		-> config.oldIDEPath
										, editorTo		= \path config	-> {config & oldIDEPath = path}
										}
							]
			,	CustomTask	(\config prevAction -> case prevAction of
								GotoPrevious = return (config, GotoPrevious)
								GotoNext =
												fileExists config.oldIDEPath
									>>= \ok.	if ok
													(return (config, GotoNext))
													(		showMessageAbout "Error" ("'" +++ config.oldIDEPath +++ "' does not exist!")
														>>|	return (config, GotoPrevious)
													)
							)
			,
				ViewOnState	"You finished the configuration of the Clean IDE!" []
			]
				

	initConfig =	{ oldIDEPath	= "..\\..\\..\\..\\..\\CleanIDE.exe"
					, projectsPath	= "projects"
					}
					
getConfig :: !(DBid AppState) -> Task IDEConfig
getConfig sid =
					readDB sid
	>>= \state.		return state.ideConfig