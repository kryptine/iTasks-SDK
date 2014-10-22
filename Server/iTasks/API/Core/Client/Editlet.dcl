definition module iTasks.API.Core.Client.Editlet

import iTasks
import iTasks.API.Core.Client.Interface
import iTasks.API.Core.Client.Component

//****************************************************************************//
// Wrapper types for defining custom editor components that can process events
// that are defined server-side but run client-side
//****************************************************************************//

:: EditletEventHandlerFunc a :== ComponentEventHandlerFunc ComponentId a // Where :: ComponentEventHandlerFunc idtype a :== idtype {JSObj JSEvent} a *JSWorld -> *(!a, !*JSWorld)
:: EditletEvent a            :== ComponentEvent ComponentId a
:: EditletHTML a             :== ComponentHTML ComponentId a
:: GenUI a                   :== ComponentId *World -> *(EditletHTML a, *World)

:: Editlet sv d
  = E.cl:
  { currVal   :: sv
  , genUI     :: GenUI cl
  , serverDef :: EditletDef d sv *World
  , clientDef :: EditletDef d cl *JSWorld
  }

:: EditletDef d s w =
  {  performIO :: ComponentId (Maybe d) s w -> *(s, w)
  ,  defVal    :: s
  ,  genDiff   :: s s -> Maybe d
  ,  appDiff   :: d s -> s
  }

:: EditletSimpl a d = EditletSimpl a (EditletSimplDef a d)

:: EditletSimplDef a d =
  {  genUI    :: GenUI a
  ,  updateUI :: ComponentId (Maybe d) a *JSWorld -> *(!a, !*JSWorld)
  ,  genDiff  :: a a -> Maybe d
  ,  appDiff  :: d a -> a
  }

toEditlet :: (EditletSimpl a d) -> (Editlet a d) | iTask a

createEditletEventHandler :: (EditletEventHandlerFunc a) !ComponentId -> JSFun b

derive JSONEncode Editlet
derive JSONDecode Editlet
derive gDefault   Editlet
derive gEq        Editlet
derive gText      Editlet
derive gEditor    Editlet
derive gEditMeta  Editlet
derive gUpdate    Editlet
derive gVerify    Editlet

