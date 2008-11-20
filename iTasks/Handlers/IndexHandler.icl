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
			 ,"js/itasks.util.js"
			 ,"js/itasks.LoginWindow.js"
			 ,"js/itasks.LoaderWindow.js"
			 ,"js/itasks.CurrentWorkPanel.js"
			 ,"js/itasks.NewWorkPanel.js"
			 ,"js/itasks.DebugPanel.js"
			 ,"js/itasks.WorkListPanel.js"
			 ,"js/itasks.HomeTabPanel.js"
			 ,"js/itasks.WorkTabPanel.js"
			 ,"js/itasks.WorkTabsPanel.js"
			 ,"js/itasks.ApplicationPanel.js"
			 ,"js/itasks.Application.js"
			 ,"js/itasks.js"
			 ]
			 
mkCssLibs = [LinkTag [RelAttr "stylesheet", TypeAttr "text/css", HrefAttr src] [] \\ src <- csslibs]
where
	csslibs =["ext/resources/css/ext-all.css"
			 ,"ext/resources/css/xtheme-gray.css"
			 ,"css/itasks.css"
			 ]

mkStartCss = [StyleTag [TypeAttr "text/css"] [Text htmlStyle, Text bgStyle]]
where
	htmlStyle = "html, body {font: normal 12px Verdana; margin: 0; padding: 0; border: 0 none; overflow: hidden; height: 100%;} "
	bgStyle = ".bg {background: #3a81ad url('img/body.png') top repeat-x;} "