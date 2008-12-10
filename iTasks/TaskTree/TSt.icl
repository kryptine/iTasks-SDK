implementation module TSt

import InternaliTasksCommon
import StdEnv, StdMaybe
import HSt

mkTst :: !UserId !Lifespan !Lifespan !*HSt -> *TSt
mkTst thisUser itaskstorage threadstorage hst
	=	{ tasknr		= [-1]
		, activated 	= True
		, staticInfo	= initStaticInfo thisUser threadstorage
		, userId		= defaultUser
		, workflowLink	= (0,(defaultUser,0,defaultWorkflowName))
		, html 			= BT [] []
		, hst 			= hst
		, trace			= False
		, options 		= initialOptions itaskstorage
		}

initStaticInfo :: UserId !Lifespan -> StaticInfo
initStaticInfo thisUser location
	=	{ currentUserId	= thisUser 
		, threadTableLoc= location
		}

initialOptions :: !Lifespan  -> Options 
initialOptions location 
	=	{ tasklife 		= location 
		, taskstorage 	= PlainString
		, taskmode 		= Edit 
		, gc			= Collect
		}

appTaskTSt :: !(Task a) !*TSt -> (!a,!*TSt)
appTaskTSt (Task fn) tst = fn tst

incTaskNr :: !TaskNr -> TaskNr
incTaskNr [] = [0]
incTaskNr [i:is] = [i+1:is]

taskNrToString :: !TaskNr -> String
taskNrToString [] 		= ""
taskNrToString [i] 		= toString i
taskNrToString [i:is] 	= taskNrToString is <+++ "." <+++ toString i 

taskNrFromString :: !String -> TaskNr
taskNrFromString "" 		= []
taskNrFromString string	= reverse (parseTaskNr` [char \\ char <-: string])
where
	parseTaskNr` :: ![Char] -> TaskNr
	parseTaskNr` [] = []
	parseTaskNr` list 
	# (front,end)	= span (\c -> c <> '.') list
	=  [toInt (toString  front) : parseTaskNr` (stl end)]

	toString :: [Char] -> String
	toString list = {c \\ c <- list}

	stl :: [Char] -> [Char]
	stl [] = []
	stl xs = tl xs
