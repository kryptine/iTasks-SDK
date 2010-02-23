module testFlow
 
import 	iTasks, CommonDomain, GeoDomain

import ShowFormFlow, FormEditor, FlowEditor, LaunchFlow


Start w = startEngine 	[ showStoredDefinitions
						, formEditor
						, flowEditor
						, launchFlow
						] w