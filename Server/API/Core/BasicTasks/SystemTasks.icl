implementation module SystemTasks

import StdList, StdArray

from TSt import :: Task, :: TSt(..), :: IWorld(..), :: Store, :: HTTPRequest, :: Config, :: StaticInfo(..), :: Workflow
from TSt import mkInstantTask, mkMonitorTask, accWorldTSt, appWorldTSt, getConfigSetting

import Types, Config
from TaskTree import :: TaskTree, :: TaskInfo,  :: TaskPriority(..), ::TaskParallelType(..), :: TreeType(..)
from TaskTree import :: TaskProperties(..), :: SystemProperties(..), :: WorkerProperties, :: ManagerProperties(..)

from Time	import :: Timestamp, :: Clock(..), clock
from Random	import genRandInt
import Text

import UserDB

from ProcessDB import :: Menu

from	iTasks import class iTask
import	GenVisualize, GenUpdate

from Email import qualified sendEmail
from Email import :: Email(..), :: EmailOption(..)

class emailOf r where emailOf :: r -> EmailAddress
instance emailOf EmailAddress where emailOf e = e
instance emailOf String where emailOf s = EmailAddress s
instance emailOf User
where
	emailOf (NamedUser n)		= EmailAddress (userName (NamedUser n))
	emailOf (RegisteredUser d)	= d.emailAddress
	emailOf RootUser			= EmailAddress ""
	emailOf AnyUser				= EmailAddress ""

getCurrentUser :: Task User
getCurrentUser = mkInstantTask "Get current user" "Determine the currently logged in user." getCurrentUser`
where
	getCurrentUser` tst=:{staticInfo}
		= (TaskFinished staticInfo.currentSession.user,tst)

getCurrentProcessId :: Task ProcessId
getCurrentProcessId = mkInstantTask "Get current process id" "Determine the process identifier of the current task instance." getCurrentProcessId`
where
	getCurrentProcessId` tst=:{staticInfo}
		= (TaskFinished staticInfo.currentProcessId,tst)

getContextWorker :: Task User
getContextWorker = mkInstantTask "Get context worker" "Determine the worker assigned to the current task." getContextWorker`
where
	getContextWorker` tst=:{TSt|properties} = (TaskFinished properties.managerProperties.worker,tst)

getContextManager :: Task User
getContextManager = mkInstantTask "Get context manager" "Determine the manager of the current task." getContextManager`
where
	getContextManager` tst=:{TSt|properties} = (TaskFinished properties.systemProperties.manager, tst)

getDefaultValue :: Task a | iTask a
getDefaultValue = mkInstantTask "Create default value" "Create a default data value." getDefaultValue`
where
	getDefaultValue` tst=:{TSt|iworld}
		# (d,iworld)	= defaultValue iworld
		= (TaskFinished d, {TSt|tst & iworld = iworld})


getRandomInt :: Task Int
getRandomInt = mkInstantTask "Create random integer" "Create a random number." getRandomInt`
where
	getRandomInt` tst
		# (Clock seed, tst)	= accWorldTSt clock tst
		= (TaskFinished (hd (genRandInt seed)), tst)

sendEmail :: !String !Note ![recipient] -> Task [recipient]	| emailOf recipient & iTask recipient
sendEmail subject (Note body) recipients = mkInstantTask "Send e-mail" "Send out an e-mail" sendEmail`
where
	sendEmail` tst=:{properties}
		//Find out the user details of the sending user
		# (mbUser,tst)	= getUserDetails properties.managerProperties.worker tst
		= case mbUser of
			Just user
				# (server,tst)	= getConfigSetting (\config -> config.smtpServer) tst
				# tst 			= foldr (sendSingle server user.emailAddress) tst (map emailOf recipients)
				= (TaskFinished recipients, tst)
			Nothing
				= (TaskException (dynamic "sendEmail: No e-mail address defined for the current user"),tst)
				
	sendSingle server (EmailAddress sender) (EmailAddress address) tst
		//For correct e-mail addresses send immediately
		| indexOf "@" address <> -1
			= sendSingle` server sender (EmailAddress address) tst
		//Lookup user details
		# (mbUser,tst)	= getUserDetails (NamedUser address) tst
		= case mbUser of
			(Just user)	= sendSingle` server sender user.emailAddress tst //Send
			Nothing		= tst //Don't send
			
	sendSingle` server sender (EmailAddress address) tst	
		# (_,tst)	= accWorldTSt ('Email'.sendEmail [EmailOptSMTPServer server]
						{email_from = sender
						,email_to = address
						,email_subject = subject
						,email_body = body
						}) tst
		= tst					
		