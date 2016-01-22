implementation module iTasks.UI.Prompt

import StdOverloaded, StdString
import qualified Data.Map as DM
import Data.Maybe

import iTasks.UI.Definition
from iTasks.API.Core.Types import TITLE_ATTRIBUTE

instance toPrompt ()
where toPrompt _ = ui (UIEmpty)

instance toPrompt String
where toPrompt hint = uic (UIEditor {UIEditor|optional=False}) [createPrompt hint]
	
instance toPrompt (!String,!String)
where toPrompt (title,hint) = uiac (UIEditor {UIEditor|optional=False}) ('DM'.fromList [(TITLE_ATTRIBUTE,title)]) [createPrompt hint]

createPrompt :: String -> UI
createPrompt hint = uic (UIContainer sizeOpts containerOpts) [stringDisplay hint]
where
	sizeOpts = {defaultSizeOpts & margins = Just {top= 5, right = 5, bottom = 10, left = 5}
			   , width = Just FlexSize, minWidth = Just WrapBound, height = Just WrapSize}
    containerOpts = {UIContainerOpts|defaultContainerOpts & baseCls=Just "itwc-prompt"}


