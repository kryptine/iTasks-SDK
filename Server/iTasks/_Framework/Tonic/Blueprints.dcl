definition module iTasks._Framework.Tonic.Blueprints

import iTasks._Framework.Tonic.Types

getTonicFunc :: !TonicModule !String -> Maybe TonicFunc

getTonicModules` :: !*IWorld -> *(!MaybeError (Dynamic, String) [String], !*IWorld)

getModule` :: !String !*IWorld -> *(!MaybeError (Dynamic, String) TonicModule, !*IWorld)

allBlueprints :: Task AllBlueprints

getModule :: !String -> Task TonicModule

getTonicModules :: Task [String]

getTonicDir :: !*IWorld -> *(!String, !*IWorld)

getTasks :: !TonicModule -> [String]
