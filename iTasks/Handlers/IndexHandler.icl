implementation module IndexHandler

import StdEnv
import Http
import Html

handleIndexRequest :: !HTTPRequest *World -> (!HTTPResponse, !*World)
handleIndexRequest req world = ({http_emptyResponse & rsp_data = toString mkPage }, world)

mkPage	= HtmlTag [] [mkHead, BodyTag [] []]
mkHead	= HeadTag [] [mkTitle : mkCssLibs ++ mkJsLibs ++ mkStartCss]
mkTitle	= TitleTag [] [Text "iTasks"]

mkJsLibs = [ScriptTag [TypeAttr "text/javascript", SrcAttr src] [] \\ src <- jslibs]
where
	jslibs = ["ext/adapter/ext/ext-base.js"
			 ,"ext/ext-all-debug.js"
			 ,"js/itasks.LoginWindow.js"
			 ,"js/itasks.LoaderWindow.js"
			 ,"js/itasks.ApplicationWindow.js"
			 ,"js/itasks.js"
			 ]
			 
mkCssLibs = [LinkTag [RelAttr "stylesheet", TypeAttr "text/css", HrefAttr src] [] \\ src <- csslibs]
where
	csslibs =["ext/resources/css/ext-all.css"
			 ,"ext/resources/css/xtheme-gray.css"
			 ,"css/itasks.css"
			 ]

mkStartCss = [StyleTag [TypeAttr "text/css"] [Text htmlStyle, Text bodyStyle ]]
where
	htmlStyle = "html, body { font: normal 12px Verdana; margin: 0; padding: 0; border: 0 none; overflow: hidden; height: 100%;} "
	bodyStyle = "body { background: #3a81ad url('img/body.png') top repeat-x;} "