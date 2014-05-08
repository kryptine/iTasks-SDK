implementation module iTasks.API.Extensions.CreateJS.CreateJS

import StdArray
from StdMisc import undef
import iTasks.API.Core.Client.Editlet
import iTasks.API.Core.Client.Interface

:: ClientState = E.a c s t:
  { actor      :: Maybe (JSVal a)
  , canvas     :: Maybe (JSVal c)
  , stage      :: Maybe (JSVal s)
  , tween      :: Maybe (JSObj t)
  }

:: TweenFlags =
  { loop :: Bool
  }

defaultTweenFlags = { loop = False }

mkGuide :: [Int] *JSWorld -> *(JSObj g, *JSWorld)
mkGuide gd world
  # (g, world) = jsEmptyObject world
  # (p, world) = jsEmptyObject world
  # world      = (p .# "path" .= toJSArg gd) world
  # world      = (g .# "guide" .= p) world
  = (g, world)

pacman :: Editlet Void Void
pacman = Editlet Void
                { EditletServerDef
                | genUI   = \cid world -> (uiDef cid, world)
                , defVal  = Void
                , genDiff = genDiffServer
                , appDiff = appDiffServer
                }
                { EditletClientDef
                | updateUI = onUpdate
                , defVal   = { actor = Nothing, canvas = Nothing, stage = Nothing, tween = Nothing }
                , genDiff  = genDiffClient
                , appDiff  = appDiffClient
                }
where
  uiDef cid
    = { html          = CanvasTag [IdAttr (canvasId cid), WidthAttr "1024px", HeightAttr "768px"]  []
      , eventHandlers = []
      , width         = ExactSize 1024
      , height        = ExactSize 768
      }

  canvasId cid = "canvas_" +++ cid

  onUpdate cid Nothing clval world
    # (obj, world) = findObject "createjs" world
    | not (jsIsUndefined obj) = onLoad cid undef clval world
    # world = addJSFromUrl "createjs-2013.12.12.min.js" (Just (createEditletEventHandler onLoad cid)) world
    = (clval,world)

  onUpdate cid _ clval world
    = (clval, world)

  onLoad cid v clval world
    # (tweenC, world) = findObject "createjs.Tween" world
    # (_, world)      = callFunction "createjs.MotionGuidePlugin.install" [toJSArg tweenC] world
    # (canvas, world) = getDomElement (canvasId cid) world
    # (stage, world)  = (new "createjs.Stage" canvas) world
    # (_, world)      = callFunction "createjs.Ticker.addEventListener" [toJSArg "tick", toJSArg stage] world
    # world           = (stage .# "autoClear" .= True) world
    # (actor, world)  = (new "createjs.Shape" ()) world
    # (ag, world)     = .? (actor .# "graphics") world
    # (_, world)      = (ag .# "setStrokeStyle" .$ (1, "round", "round")) world
    # (_, world)      = (ag .# "beginStroke" .$ "#000000") world
    # (bf, world)     = (ag .# "beginFill" .$ "#ff0000") world
    # (_, world)      = (bf .# "drawCircle" .$ (0, 0, 50)) world
    # (_, world)      = (ag .# "endStroke" .$ ()) world
    # (_, world)      = (ag .# "endFill" .$ ()) world
    # (_, world)      = (stage .# "addChild" .$ actor) world
    # (at, world)     = callFunction "createjs.Tween.get" [toJSArg actor, toJSArg { loop = True }, toJSArg True] world
    # world           = drawAgent at world
    = (clval, world)

  drawAgent at world
    # (g1, world)     = mkGuide [100,100, 800,100, 800,300] world
    # (g2, world)     = mkGuide [800,300, 100,300, 100,100] world
    # (_, world)      = (at .# "to" .$ (g1, 2500)) world
    # (_, world)      = (at .# "wait" .$ 500) world
    # (_, world)      = (at .# "to" .$ (g2, 1500)) world
    = world

    //# (clrect, world)     = (actor .# "getBoundingClientRect" .$ ()) world

  genDiffClient clval1 clval2 = Nothing

  genDiffServer val1 val2 = Nothing

  appDiffClient diffs clval = clval

  appDiffServer diffs val = val
