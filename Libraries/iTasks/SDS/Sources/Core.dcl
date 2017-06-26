definition module iTasks.SDS.Sources.Core
/*
* This module provides the builtin shared sources
*/
from iTasks.SDS.Definition import :: SDS
from System.FilePath import :: FilePath

// constant share from which you always read the same value
constShare	            :: !a -> SDS p a ()

// null share to which you can write anything
nullShare		        :: SDS p () a

// Random source
randomInt				:: SDS () Int ()

// External file
externalFile            :: SDS FilePath String String

// External directory
externalDirectory       :: SDS FilePath [FilePath] ()
