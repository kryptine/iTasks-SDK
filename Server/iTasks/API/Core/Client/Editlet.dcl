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
:: EditletHTML                 :== ComponentHTML

:: Editlet sv d
  = E.cl f:
  { currVal    :: sv // TODO: implementation detail, remove it

  // This field is unnecessary, gDefault could be used instead of it
  // However, Jurrien like it, so why not to be here :)
  , defValSrv  :: sv

  , genUI      :: ComponentId *World -> *(EditletHTML, *World)
  , initClient :: ((EditletEventHandlerFunc d cl) ComponentId -> JSFun f) ComponentId *JSWorld -> *(cl, *JSWorld)
  , appDiffClt :: ((EditletEventHandlerFunc d cl) ComponentId -> JSFun f) ComponentId d cl *JSWorld -> *(cl, *JSWorld)
  , genDiffSrv :: sv sv -> Maybe d
  , appDiffSrv :: d  sv -> sv
  }

derive JSONEncode Editlet
derive JSONDecode Editlet
derive gDefault   Editlet
derive gEq        Editlet
derive gText      Editlet
derive gEditor    Editlet
derive gEditMeta  Editlet
derive gUpdate    Editlet
derive gVerify    Editlet

