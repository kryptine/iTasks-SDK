implementation module iTasks.Extensions.Currency
import iTasks
import iTasks.UI.Editor.Controls, iTasks.UI.Editor.Modifiers
import Text, Data.Maybe, Data.Func
import qualified Data.Map as DM

//* Money (ISO4217 currency codes are used)
gText{|EUR|} _ val = [maybe "" toString val]

gEditor{|EUR|} = selectByMode eurView eurField eurField 
where
	fromEUR (EUR v) = toReal v / 100.0
	toEUR v = EUR (toInt (100.0 * v))

	eurView = ignoreEditorWrites $ comapEditorValue toString textView
	eurField
		= bijectEditorWrite fromEUR toEUR
		$ bijectEditorValue fromEUR toEUR 
		$ withDynamicHintAttributes "amount in EUR" decimalField

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

gEditor{|USD|} = selectByMode usdView usdField usdField 
where
	fromUSD (USD v) = toReal v / 100.0
	toUSD v = USD (toInt (100.0 * v))

	usdView = ignoreEditorWrites $ comapEditorValue toString textView
	usdField
		= bijectEditorWrite fromUSD toUSD
		$ bijectEditorValue fromUSD toUSD
		$ withDynamicHintAttributes "amount in USD" decimalField

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
