definition module Setup

import HTTP, HttpServer, Config

setupHandler :: !(Config -> [(String -> Bool, (HTTPRequest *World -> *(!HTTPResponse,!HTTPServerControl,!*World)))]) !HTTPRequest !*World -> (!HTTPResponse, !HTTPServerControl, !*World)