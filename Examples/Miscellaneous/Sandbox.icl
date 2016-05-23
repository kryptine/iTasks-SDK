module Sandbox

import iTasks, iTasks.API.Extensions.SandBox

inSandBox :: Task String
inSandBox = updateInformation "Example of execution in sandbox" [] "Hello World"

example = withShared defaultValue
	\input ->
		withShared defaultValue 
	\output ->	
		withShared defaultValue 
	\result ->
		evalSandBoxed input output result inSandBox 

Start w = startEngine example w
