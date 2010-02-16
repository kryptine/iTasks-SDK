module testFlow
 
import 	iTasks, CommonDomain, GeoDomain

import FormFlowStorage, FormEditor, FlowEditor, LaunchFlow


Start w = startEngine 	[ showStoredDefinitions
						, formEditor
						, flowEditor
						, launchFlow
						] w