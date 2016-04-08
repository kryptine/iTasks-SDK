implementation module iTasks.UI.Prompt

import StdOverloaded, StdString
import qualified Data.Map as DM
import Data.Maybe
import Text.JSON

import iTasks.UI.Definition
from iTasks.API.Core.Types import TITLE_ATTRIBUTE
from StdFunc import o

instance toPrompt ()
where toPrompt _ = ui UIEmpty

instance toPrompt String
where toPrompt hint = createPrompt hint
	
instance toPrompt (!String,!String)
where toPrompt (title,hint) = setTitle title (createPrompt hint)

createPrompt :: String -> UI
createPrompt hint = style (uic UIContainer [stringDisplay hint])
where
	style = setMargins 5 5 10 5 o setWidth FlexSize o setMinWidth WrapBound o setHeight WrapSize o setBaseCls "itwc-prompt"
