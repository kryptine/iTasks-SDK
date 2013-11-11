implementation module iTasks.Framework.GenSpecialize
import Text.JSON, Data.Functor
import iTasks.Framework.Generic

customJSONEncode :: (a -> b) a -> [JSONNode] | JSONEncode{|*|} b
customJSONEncode toPrj a = JSONEncode{|*|} (toPrj a)

customJSONDecode :: (b -> a) [JSONNode] -> (Maybe a,![JSONNode]) | JSONDecode{|*|} b
customJSONDecode fromPrj inp = let (mbb,rem) = JSONDecode{|*|} inp in (fmap fromPrj mbb,rem)

customGVisualizeText :: (a -> b) !VisualizationFormat !a -> [String] | gVisualizeText{|*|} b
customGVisualizeText toPrj format val = gVisualizeText{|*|} format (toPrj val)

customGEditor :: (a -> b) DataPath (VerifiedValue a) [EditMeta] !*VSt -> (!VisualizationResult,!*VSt) | gEditor{|*|} b
customGEditor toPrj dp (mba,mask,ver) meta vst = gEditor{|*|} dp (toPrj mba,mask,ver) meta vst

customGUpdate :: (a -> b) (b -> a) ![Int] !JSONNode !(!a,!InteractionMask) !*USt -> (!(!a,!InteractionMask),!*USt) | gUpdate{|*|} b
customGUpdate toPrj fromPrj path upd (a,amask) ust
    # ((b,bmask),ust) = gUpdate{|*|} path upd (toPrj a,amask) ust
    = ((fromPrj b, bmask),ust)
