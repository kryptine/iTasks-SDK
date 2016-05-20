implementation module iTasks.UI.Prompt

import StdOverloaded, StdString
import qualified Data.Map as DM
import Data.Maybe
import Text.JSON

import iTasks.UI.Definition
from StdFunc import o

instance toPrompt ()
where toPrompt _ = ui UIEmpty

instance toPrompt String
where toPrompt hint = createPrompt hint
	
instance toPrompt (!String,!String)
where toPrompt (title,hint) = let (UI type attr items) = createPrompt hint in
		(UI type ('DM'.union (titleAttr title) attr) items)

createPrompt :: String -> UI
createPrompt hint = (uiac UIContainer attr [stringDisplay hint])
where
	attr = 'DM'.unions [marginsAttr 5 5 10 5, widthAttr FlexSize, minWidthAttr WrapBound, heightAttr WrapSize, baseClsAttr "itasks-prompt"]
