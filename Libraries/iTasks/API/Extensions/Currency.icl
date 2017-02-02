implementation module iTasks.API.Extensions.Currency
import iTasks
import iTasks.UI.Editor.Builtin, iTasks.UI.Editor.Combinators
import Text
import qualified Data.Map as DM

//* Money (ISO4217 currency codes are used)
gText{|EUR|} _ val = [maybe "" toString val]

gEditor{|EUR|} = whenDisabled
		(liftEditor toString (\_ -> EUR 0) (textView 'DM'.newMap))
		(liftEditor (\(EUR v) -> toReal v / 100.0) (\v -> EUR (toInt (100.0 * v))) (withHintAttributes "amount in EUR" (decimalField 'DM'.newMap)))

instance toString EUR
where
	toString c = "EUR " +++ decFormat (toInt c)
	
instance + EUR
where
	(+) (EUR x) (EUR y) = EUR (x + y)

instance - EUR
where
	(-) (EUR x) (EUR y) = EUR (x - y)

instance == EUR
where
	(==) (EUR x) (EUR y) = x == y

instance < EUR
where
	(<) (EUR x) (EUR y) = x < y

instance toInt EUR
where
	toInt (EUR val) = val

instance zero EUR
where
	zero = EUR 0

gText{|USD|} _ val = [maybe "" toString val]

gEditor{|USD|} = whenDisabled
		(liftEditor toString (\_ -> USD 0) (textView 'DM'.newMap))
		(liftEditor (\(USD v) -> toReal v / 100.0) (\v -> USD (toInt (100.0 * v))) (withHintAttributes "amount in USD" (decimalField 'DM'.newMap)))

instance toString USD
where
	toString c = "USD " +++ decFormat (toInt c)

instance + USD
where
	(+) (USD x) (USD y) = USD (x + y)

instance - USD
where
	(-) (USD x) (USD y) = USD (x - y)

instance == USD
where
	(==) (USD x) (USD y) = x == y

instance < USD
where
	(<) (USD x) (USD y) = x < y
	
instance toInt USD
where
	toInt (USD val) = val

instance zero USD
where
	zero = USD 0

derive JSONEncode		EUR, USD
derive JSONDecode		EUR, USD
derive gDefault			EUR, USD
derive gEq				EUR, USD

decFormat x = toString (x / 100) +++ "." +++ lpad (toString (x rem 100)) 2 '0'
