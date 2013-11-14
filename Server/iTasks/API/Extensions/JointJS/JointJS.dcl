definition module iTasks.API.Extensions.JointJS.JointJS

import iTasks
import iTasks.API.Core.Client.Editlet
import iTasks.API.Core.Client.Interface

:: JointJS = JointJS
:: JointJSDiff = JointJSDiff

derive class iTask JointJS, JointJSDiff

jointJSEditlet :: JointJS -> Editlet JointJS [JointJSDiff]
