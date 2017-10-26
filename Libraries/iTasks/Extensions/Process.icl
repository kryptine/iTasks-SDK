implementation module iTasks.Extensions.Process

import iTasks.WF.Definition
import iTasks.WF.Tasks.Core
import iTasks.WF.Tasks.IO
import iTasks.WF.Tasks.Interaction
import iTasks.SDS.Sources.Core

import StdString
import Data.Maybe, Data.Error
import qualified System.Process

derive class iTask ProcessStatus, CallException

instance toString CallException
where
	toString (CallFailed (_,err)) = "Error calling external process: " +++ err

callProcess :: !d ![ViewOption ProcessStatus] !FilePath ![String] !(Maybe FilePath) -> Task ProcessStatus | toPrompt d
callProcess prompt viewOptions executable arguments workingDirectory
	= externalProcess executable arguments workingDirectory unitShare handlers gEditor{|*|}
where
	handlers = {onStartup = onStartup, onOutData = onOutData, onErrData = onErrData, onShareChange = onShareChange, onExit = onExit}

	onStartup _                    = (Ok (RunningProcess executable), Nothing, [], False)
	onOutData data status _        = (Ok status, Nothing, [], False)
	onErrData data status _        = (Ok status, Nothing, [], False)
    onShareChange status _         = (Ok status, Nothing, [], False)
	onExit (ExitCode exitCode) _ _ = (Ok (CompletedProcess exitCode), Nothing) 

callInstantProcess :: !FilePath ![String] !(Maybe FilePath) -> Task Int
callInstantProcess cmd args dir = accWorldError (\world -> 'System.Process'.callProcess cmd args dir world) CallFailed 
