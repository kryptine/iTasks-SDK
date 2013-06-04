definition module iTasks.Gin.ORYXExtensions

from Data.Maybe import :: Maybe
from Text.JSON import generic JSONEncode, generic JSONDecode, :: JSONNode

:: ORYXExtensionsFile =
    { extensions    :: ![ORYXExtension]
    , perspectives  :: ![ORYXPerspective]
    }

:: ORYXExtension =
    { title         :: !String
    , namespace     :: !String
    , description   :: !String
    , definition_   :: !String
    , extends       :: !String
    }

:: ORYXPerspective =
    { title             :: !String
    , namespace         :: !String
    , description       :: !String
    , stencilset        :: !String
    , addExtensions     :: ![String]
    , removeExtensions  :: ![String]
    }

derive JSONEncode  ORYXExtensionsFile, ORYXExtension, ORYXPerspective
derive JSONDecode  ORYXExtensionsFile, ORYXExtension, ORYXPerspective

makeORYXExtensionsFile :: ![String] -> ORYXExtensionsFile
