definition module Setup

import HTTP, Config

setupHandler :: !HTTPRequest !*World -> (!HTTPResponse, !*World)