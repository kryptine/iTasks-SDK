definition module Setup

import Http, HttpServer, Config

setupHandler :: !(Config -> [(String -> Bool, (HTTPRequest *World -> *(!HTTPResponse,!HTTPServerControl,!*World)))]) !HTTPRequest !*World -> (!HTTPResponse, !HTTPServerControl, !*World)