definition module iTasks.Extensions.Email
/**
* This module provides basic SMTP email support
*/
from Text.HTML import :: HtmlTag
from TCPIP     import :: Timeout
import iTasks

/**
* Send an e-mail message.
*
* @param Options: Mail server options, when left blank port 25 on localhost is used SMTP server
* @param Sender: The sender address
* @param Recipients: The recipient addresses
* @param Subject: The subject line of the e-mail message
* @param Body: The body of the e-mail message
* @param Attachments: Attachments added to the e-mail message
*/
sendEmail :: ![EmailOpt] !String ![String] !String !String ![Attachment] -> Task ()

/**
* Send an e-mail message with HTML body.
*
* @param Options: Mail server options, when left blank port 25 on localhost is used SMTP server
* @param Sender: The sender address
* @param Recipients: The recipient addresses
* @param Subject: The subject line of the e-mail message
* @param Body: The HTML body of the e-mail message. Text has to be UTF-8 encoded.
* @param Attachments: Attachments added to the e-mail message.
*/
sendHtmlEmail :: ![EmailOpt] !String ![String] !String !HtmlTag ![Attachment] -> Task ()

//Options for sendEmail
:: EmailOpt
	= EmailOptSMTPServer !String              //SMTP server to use. Default: localhost
	| EmailOptSMTPServerPort !Int             //TCP port of the SMTP server to use. Default: 25
	| EmailOptExtraHeaders ![(String,String)] //Additional headers to add before the body
	| EmailOptTimeout !Timeout                // TCP timeout

//* Email attachment.
:: Attachment =
	{ name    :: !String  //* The attachment's filename.
	, content :: !{#Char} //* Content of the attachment, arbitrary binary data.
	}
