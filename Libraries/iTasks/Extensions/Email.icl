implementation module iTasks.Extensions.Email

import StdEnv
import Data.Functor, Data.Func
import Text, Text.HTML, Text.Encodings.Base64
import iTasks

sendEmail :: ![EmailOpt] !String ![String] !String !String ![Attachment] -> Task ()
sendEmail opts sender recipients subject body attachments
	= tcpconnect server port timeout (constShare ()) {ConnectionHandlers|onConnect=onConnect,onData=onData,onDisconnect=onDisconnect,onShareChange = \l _ = (Ok l, Nothing, [], False), onDestroy= \s->(Ok s, [])}
	@! ()
where
	server 	= getServerOpt opts
	port	= getPortOpt opts
	timeout = getTimeoutOpt opts
	headers = getHeadersOpt opts
	//Sending the message with SMTP is essentially one-way communication
	//but we send it in parts. After each part we get a response with a status code.
	//After each message we check if it is a status code we expect.
	messages =
			[("",220) //Initially we don't send anything, but wait for the welcome message from the server
			,(smtpHelo, 250)
			,(smtpFrom sender, 250)
			]
		++
			((\recipient -> (smtpTo recipient, 250)) <$> recipients)
		++
			[(smtpData, 354)
			,(smtpBody sender recipients headers subject body attachments, 250)
			,(smtpQuit, 221)
			]

	//Send the first message
	onConnect :: !ConnectionId !String !() -> (!MaybeErrorString [(String, Int)], !Maybe (), ![String], !Bool)
    onConnect _ _ _
        = (Ok messages,Nothing,[],False)
	//Response to last message: if ok, close connection
	onData :: !String ![(String, Int)] !() -> (!MaybeErrorString [(String, Int)], !Maybe (), ![String], !Bool)
    onData data [(_,expectedCode)] _
		| statusCode data == expectedCode
			= (Ok [],Nothing,[],True)
        	= (Error data,Nothing,[],False)
	//Response to other messages: if ok, send next message
    onData data [(_,expectedCode):ms] _
		| statusCode data == expectedCode
        	= (Ok ms,Nothing,[fst (hd ms)],False)
        	= (Error data,Nothing,[],False)

	//We don't expect the server to disconnect before we close
	//the connection ourselves
    onDisconnect _ _
		= (Error "SMTP server disconnected unexpectedly",Nothing)

sendHtmlEmail :: ![EmailOpt] !String ![String] !String !HtmlTag ![Attachment] -> Task ()
sendHtmlEmail opts sender recipients subject body attachments =
	sendEmail
		[EmailOptExtraHeaders [("content-type", "text/html; charset=UTF8")]: opts]
		sender
		recipients
		subject
		htmlString
		attachments
where
	// avoid too long lines (SMTP allows a max length of 1000 characters only)
	// by inserting a newline (\r\n is required for mails) after each tag
	htmlString = replaceSubString ">" ">\r\n" $ toString body

// SMTP messages
smtpHelo = "HELO localhost\r\n"
smtpFrom email_from = "MAIL FROM:<" +++ (cleanupEmailString email_from) +++ ">\r\n"
smtpTo email_to = "RCPT TO:<" +++ (cleanupEmailString email_to) +++ ">\r\n"
smtpData = "DATA\r\n"
smtpBody email_from email_to bodyHeaders email_subject email_body attachments =
	concat $ flatten $
			[ [k, ":", v, "\r\n"]
			\\ (k, v) <-
					[ ("From", cleanupEmailString email_from)
					: (\email_to -> ("To", cleanupEmailString email_to)) <$> email_to
					]
				++
					[("Subject", cleanupEmailString email_subject)]
			]
		++
			if (isEmpty attachments) [] [["Content-Type: multipart/mixed; boundary=sep\r\n--sep\r\n"]]
		++
			[[k, ":", v, "\r\n"] \\ (k, v) <- bodyHeaders]
		++
			[["\r\n", email_body, "\r\n"], if (isEmpty attachments) [] ["--sep"], ["\r\n"]]
		++
			[ flatten $
				[	[ "content-type: application/octet-stream; name=\"", attachment.Attachment.name, "\"\r\n"
					, "content-disposition: attachment; filename=\"", attachment.Attachment.name, "\"\r\n"
					, "content-transfer-encoding: base64\r\n"
					, "\r\n"
					]
				,	withRestrictedLineLength (base64Encode attachment.content)
				,	["\r\n--sep\r\n"]
				]
			\\ attachment <- attachments
			]
		++
			[[".\r\n"]]
smtpQuit = "QUIT\r\n"

//Utility functions

//Parse the reply of the server into a status code
statusCode :: String -> Int
statusCode msg = toInt (msg % (0,2))

//Strip any newline chars and tabs from a string.
cleanupEmailString :: String -> String
cleanupEmailString s = toString (filter (\x -> not (isMember x ['\r\n\t'])) (fromString s))

getServerOpt [] 						= "localhost"
getServerOpt [EmailOptSMTPServer s:xs]	= s
getServerOpt [x:xs] 					= getServerOpt xs

getPortOpt [] 						= 25 
getPortOpt [EmailOptSMTPServerPort s:xs]	= s
getPortOpt [x:xs] 					= getPortOpt xs

getHeadersOpt [] 							= []
getHeadersOpt [EmailOptExtraHeaders s:xs]	= s ++ getHeadersOpt xs
getHeadersOpt [x:xs] 						= getHeadersOpt xs

getTimeoutOpt []                     = Nothing
getTimeoutOpt [EmailOptTimeout t:xs] = Just t
getTimeoutOpt [x:xs]                 = getTimeoutOpt xs

//* Cut into lines of 1000 character (including "\r\n"), to fulfil SMTP standard.
withRestrictedLineLength :: !String -> [String]
withRestrictedLineLength str = reverse $ withRestrictedLineLength` 0 []
where
	withRestrictedLineLength` i acc
		| strSize - i <= 998 = [str % (i, strSize - 1): acc]
		| otherwise          = withRestrictedLineLength` (i + 998) ["\r\n", str % (i, i + 997): acc]

	strSize = size str
