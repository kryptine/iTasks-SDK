definition module Setup

import Http

setupHandler :: !HTTPRequest !*World -> (!HTTPResponse, !*World)