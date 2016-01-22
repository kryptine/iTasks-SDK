definition module iTasks.UI.Prompt
/**
* This module provides different ways to create prompts
*/
from iTasks.UI.Definition import :: UI

class toPrompt d :: !d -> UI

instance toPrompt ()                  //No prompt
instance toPrompt String              //Simple instruction
instance toPrompt (!String, !String)  //Title attribute + instruction
