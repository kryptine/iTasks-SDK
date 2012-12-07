implementation module IDE_State

/* Global Shared State of the IDE
*/

import iTasks

import projectManager, SmallUtil

// The IDE_state is the one and only globally shared state

derive class iTask IDE_State, Module, PaneOptions 

init_IDE_State :: IDE_State			
init_IDE_State
	= 	{ projectName		= ""
		, projectPath 		= initialPath
		, projectSettings	= PR_InitProject
		, cleanPath			= cleanPath 
		, openedFiles		= []
		, recentFiles 		= []
		, recentProjects	= []
		, idx				= 0
		, envTargets		= [t_StdEnv]
		, allFilesInEnv		= []
		, projectPaneOption = ShowAll
		}

IDE_State :: Shared IDE_State
IDE_State = sharedStore IDE_State_fileName init_IDE_State

update_IDE_State :: !(IDE_State -> IDE_State) -> Task !Void
update_IDE_State fun = update fun IDE_State @ const Void

get_IDE_State :: Task IDE_State
get_IDE_State = get IDE_State 

watch_IDE_State :: !(IDE_State -> Bool) !(Task a) -> Task a | iTask a
watch_IDE_State pred task = watch IDE_State >>* [OnValue (pred o getValue) (const task)]

// updating the global IDE_State

set_new_Project :: !ModuleName !ProjectPath -> Task Void
set_new_Project moduleName projectPath 
	=						open_Project moduleName projectPath (initProject moduleName) 

open_Project :: !ModuleName !ProjectPath !Project -> Task Void
open_Project projectName projectPath project
	=	update_IDE_State
				(\state -> 	{ state	& projectName						= projectName
									, projectPath						= projectPath
							     	, projectSettings					= project
							     	, recentProjects 					= removeDup [projectName:state.recentProjects]
							})
	>>|
		update_IDE_State
				(\state -> 	{ state & projectSettings.root_directory   	= projectPath
									, projectSettings.target 			= (state.envTargets!!state.idx).target_name
							})
	
update_Project :: !Project -> Task Void
update_Project project
	=	update_IDE_State
			 (\state -> 	{ state & projectSettings 					= project
							}) 

setProjectPaneOption :: !PaneOptions -> Task Void
setProjectPaneOption option
	= 	update_IDE_State
			 (\state -> 	{ state & projectPaneOption					= option		
							}) 	

setEnvironments :: ![Target] -> Task Void
setEnvironments envs
	= 	update_IDE_State
			 (\state -> 	{ state & envTargets						= envs		
							}) 	
add_Environments :: ![Target] -> Task Void
add_Environments env
	= 	update_IDE_State
			 (\state -> 	{ state & envTargets						= state.envTargets ++ env	
							}) 	
updateEnvironment :: !Int !Target -> Task Void
updateEnvironment  idx target 		
	= 	update_IDE_State
			 (\state -> 	{ state & idx								= idx
									, projectSettings.target			= (state.envTargets!!idx).target_name
									, envTargets 						= updateAt idx target state.envTargets
//									, projectSettings.prjpaths 			= (state.envTargets!!idx).target_path 	// not needed yet ???
							}) 	
select_Environment :: !Int -> Task Void
select_Environment idx 	
	= 	update_IDE_State
			 (\state -> 	{ state & idx								= idx
									, projectSettings.target			= (state.envTargets!!idx).target_name
//									, projectSettings.prjpaths 			= (state.envTargets!!idx).target_path 
							}) 	

setAllFilesInEnv	:: ![(!DirPathName,![Module])] -> Task Void
setAllFilesInEnv all
	= 	update_IDE_State
			 (\state -> 	{ state & allFilesInEnv			= all
			 				})

addFilesAdmin :: !FileName -> Task Void
addFilesAdmin fileName
	=	update_IDE_State
			 (\state -> 	{ state & recentFiles 						= removeDup [fileName:state.recentFiles]
								    , openedFiles 						= [fileName:state.openedFiles]
							}) 
removeFileAdmin :: !FileName -> Task Void
removeFileAdmin fileName
	=	update_IDE_State
			 (\state -> 	{ state & openedFiles 						= removeMember fileName state.openedFiles
							}) 





