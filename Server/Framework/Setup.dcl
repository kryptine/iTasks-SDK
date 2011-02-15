definition module Setup

import HTTP, Config

setupHandler :: !(Config -> [(String -> Bool, (HTTPRequest *World -> *(!HTTPResponse,!*World)))]) !HTTPRequest !*World -> (!HTTPResponse, !*World)