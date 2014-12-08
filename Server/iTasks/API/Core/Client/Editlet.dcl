definition module iTasks.API.Core.Client.Editlet

import iTasks
import iTasks.API.Core.Client.Interface
import iTasks.API.Core.Client.Component

//****************************************************************************//
// Wrapper types for defining custom editor components that can process events
// that are defined server-side but run client-side
//****************************************************************************//

:: EditletEventHandlerFunc d a :== ComponentEventHandlerFunc d a
:: EditletEvent d a            :== ComponentEvent d a
:: EditletHTML d a             :== ComponentHTML d a
:: GenUI d a                   :== ComponentId *World -> *(EditletHTML d a, *World)

:: Editlet sv d
  = E.cl:
  { currVal    :: sv // TODO: implementation detail, remove it

  // These fields are unnecessary, gDefault could be used instead of them
  // However, Jurrien like them, so why not to be here :)
  , defValSrv  :: sv
  , defValClt  :: cl

  , genUI      :: GenUI d cl
  , appDiffClt :: ComponentId d cl *JSWorld -> *(cl, *JSWorld)
  , genDiffSrv :: sv sv -> Maybe d
  , appDiffSrv :: d  sv -> sv
  }

createEditletEventHandler :: (EditletEventHandlerFunc d a) !ComponentId -> JSFun b

derive JSONEncode Editlet
derive JSONDecode Editlet
derive gDefault   Editlet
derive gEq        Editlet
derive gText      Editlet
derive gEditor    Editlet
derive gEditMeta  Editlet
derive gUpdate    Editlet
derive gVerify    Editlet

