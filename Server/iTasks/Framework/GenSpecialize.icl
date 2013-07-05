implementation module iTasks.Framework.GenSpecialize
import Text.JSON, Data.Functor
import iTasks.Framework.iTaskClass

customJSONEncode :: (a -> b) a -> [JSONNode] | JSONEncode{|*|} b
customJSONEncode toPrj a = JSONEncode{|*|} (toPrj a)

customJSONDecode :: (b -> a) [JSONNode] -> (Maybe a,![JSONNode]) | JSONDecode{|*|} b
customJSONDecode fromPrj inp = let (mbb,rem) = JSONDecode{|*|} inp in (fmap fromPrj mbb,rem)

customGVisualizeText :: (a -> b) !VisualizationFormat !a -> [String] | gVisualizeText{|*|} b
customGVisualizeText toPrj format val = gVisualizeText{|*|} format (toPrj val)

customGEditor :: (a -> b) DataPath (VerifiedValue a) !*VSt -> (!VisualizationResult,!*VSt) | gEditor{|*|} b
customGEditor toPrj dp (mba,mask,ver) vst = gEditor{|*|} dp (toPrj mba,mask,ver) vst

customGUpdate :: (a -> b) (b -> a) ![Int] !JSONNode !(!a,!InteractionMask) -> (!a,!InteractionMask) | gUpdate{|*|} b
customGUpdate toPrj fromPrj path upd (a,amask)
    # (b,bmask) = gUpdate{|*|} path upd (toPrj a,amask)
    = (fromPrj b, bmask)
