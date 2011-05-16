implementation module SystemTasks

import StdList, StdArray, TSt, Types, Config, Text, UserDB

from Email import qualified sendEmail
from Email import :: Email(..), :: EmailOption(..)

getCurrentUser :: Task User
getCurrentUser = mkInstantTask ("Get current user", "Determine the currently logged in user.") getCurrentUser`
where
	getCurrentUser` tst=:{TSt|properties} = (TaskFinished properties.ProcessProperties.managerProperties.worker,tst)
	
sendEmail :: !String !Note ![EmailAddress] -> Task [EmailAddress]
sendEmail subject (Note body) recipients = mkInstantTask ("Send e-mail", "Send out an e-mail") sendEmail`
where
	sendEmail` tst=:{TSt|properties}
		//Find out the user details of the sending user
		# (mbUser,tst)	= getUserDetails properties.ProcessProperties.managerProperties.worker tst
		= case mbUser of
			Just user
				# (server,tst)	= getConfigSetting (\config -> config.smtpServer) tst
				# tst 			= foldr (sendSingle server user.emailAddress) tst recipients
				= (TaskFinished recipients, tst)
			Nothing
				= (taskException "sendEmail: No e-mail address defined for the current user",tst)
				
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
		