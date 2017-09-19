definition module iTasks.Extensions.Picture.JPEG

import iTasks
//from iTasks.API.Core.Types import :: Task, generic gEq, generic gDefault, generic JSONDecode, generic JSONEncode, generic gText, generic gEditor, :: Editor, :: TaskAttributes, :: DateTime, instance toString DateTime
//from Text.JSON import :: JSONNode, generic JSONEncode, generic JSONDecode
//from iTasks._Framework.Generic.Visualization import :: TextFormat(..)
//from Data.Maybe import :: Maybe
//from Data.Map import :: Map

:: JPEGPicture = JPEGPicture String

derive gText JPEGPicture
derive JSONEncode JPEGPicture
derive JSONDecode JPEGPicture
derive gDefault JPEGPicture
derive gEq JPEGPicture

derive gEditor JPEGPicture
