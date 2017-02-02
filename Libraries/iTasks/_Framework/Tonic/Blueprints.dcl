definition module iTasks._Framework.Tonic.Blueprints

from iTasks._Framework.Task import :: Task
from iTasks._Framework.IWorld import :: IWorld
from iTasks._Framework.Tonic.Types import :: AllBlueprints, :: TonicModule, :: TonicFunc, :: ModuleName, :: FuncName
from Data.Error import :: MaybeError
from Data.Map import :: Map
from Data.Maybe import :: Maybe

getTonicFunc :: !TonicModule !String -> Maybe TonicFunc

getTonicModules` :: !*IWorld -> *(!MaybeError (Dynamic, String) [String], !*IWorld)

getModule` :: !String !*IWorld -> *(!MaybeError (Dynamic, String) TonicModule, !*IWorld)

allBlueprints :: Task AllBlueprints

getModule :: !String -> Task TonicModule

getTonicModules :: Task [String]

getTonicDir :: !*IWorld -> *(!String, !*IWorld)

getTasks :: !TonicModule -> [String]
