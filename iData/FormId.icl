implementation module FormId

import StdBool, StdInt, StdString
import iDataSettings
import StdMaybe
import GenEq
import Html

// utility for creating FormId's

class   (<@) infixl 4 att :: !(FormId d) !att -> FormId d

instance <@ String        where <@ formId a = {FormId | formId & id       = a}
instance <@ Lifespan      where <@ formId a = {FormId | formId & lifespan = a}
instance <@ Mode          where <@ formId a = {FormId | formId & mode     = a}
instance <@ StorageFormat where <@ formId a	= {FormId | formId & storage  = a}

mkFormId :: !String !d -> FormId d				// Default FormId with given id and ival.
mkFormId s d = {id = s, issub = False, lifespan = LSPage, mode = Edit, storage = PlainString, ival = d}

initID :: !(FormId d) -> InIDataId d	// (Init,FormId a)
initID formid			= (Init,formid)

setID :: !(FormId d) !d -> InIDataId d	// (Set,FormId a)
setID formid na			= (Set,setFormId formid na)

// editable, string format

tFormId		:: !String !d -> FormId d;			tFormId    s d = mkFormId  s d <@ LSTemp
nFormId		:: !String !d -> FormId d;			nFormId    s d = mkFormId  s d <@ LSPage
sFormId		:: !String !d -> FormId d;			sFormId    s d = mkFormId  s d <@ LSSession
pFormId		:: !String !d -> FormId d;			pFormId    s d = mkFormId  s d <@ LSTxtFile
rFormId		:: !String !d -> FormId d;			rFormId    s d = mkFormId  s d <@ LSTxtFileRO

tdFormId	:: !String !d -> FormId d;			tdFormId   s d = tFormId   s d <@ Display
ndFormId	:: !String !d -> FormId d;			ndFormId   s d = nFormId   s d <@ Display
sdFormId	:: !String !d -> FormId d;			sdFormId   s d = sFormId   s d <@ Display
pdFormId	:: !String !d -> FormId d;			pdFormId   s d = pFormId   s d <@ Display
rdFormId	:: !String !d -> FormId d;			rdFormId   s d = rFormId   s d <@ Display

xtFormId	:: !String !d -> FormId d;			xtFormId   s d = tFormId   s d <@ NoForm
xnFormId	:: !String !d -> FormId d;			xnFormId   s d = nFormId   s d <@ NoForm
xsFormId	:: !String !d -> FormId d;			xsFormId   s d = sFormId   s d <@ NoForm
xpFormId	:: !String !d -> FormId d;			xpFormId   s d = pFormId   s d <@ NoForm
xrFormId	:: !String !d -> FormId d;			xrFormId   s d = rFormId   s d <@ NoForm

nDFormId	:: !String !d -> FormId d;			nDFormId   s d = nFormId   s d <@ StaticDynamic
sDFormId	:: !String !d -> FormId d;			sDFormId   s d = sFormId   s d <@ StaticDynamic
pDFormId	:: !String !d -> FormId d;			pDFormId   s d = pFormId   s d <@ StaticDynamic
rDFormId	:: !String !d -> FormId d;			rDFormId   s d = rFormId   s d <@ StaticDynamic

ndDFormId	:: !String !d -> FormId d;			ndDFormId  s d = nDFormId  s d <@ Display
sdDFormId	:: !String !d -> FormId d;			sdDFormId  s d = sDFormId  s d <@ Display
pdDFormId	:: !String !d -> FormId d;			pdDFormId  s d = pDFormId  s d <@ Display
rdDFormId	:: !String !d -> FormId d;			rdDFormId  s d = rDFormId  s d <@ Display

extidFormId :: !(FormId d) !String -> FormId d
extidFormId formid s	= formid <@ formid.id +++ iDataIdSeparator +++ s

subFormId :: !(FormId a) !String !d -> FormId d			// make new formid of new type copying other old settinf
subFormId formid s d	= {reuseFormId (extidFormId formid s) d & issub = True}

subnFormId :: !(FormId a) !String !d -> FormId d		// make new formid of new type copying other old settinf
subnFormId formid s d	= subFormId formid s d <@ LSPage

subsFormId :: !(FormId a) !String !d -> FormId d		// make new formid of new type copying other old settinf
subsFormId formid s d	= subFormId formid s d <@ LSSession

subpFormId :: !(FormId a) !String !d -> FormId d		// make new formid of new type copying other old settinf
subpFormId formid s d	= subFormId formid s d <@ LSTxtFile

subtFormId :: !(FormId a) !String !d -> FormId d		// make new formid of new type copying other old settinf
subtFormId formid s d	= subFormId formid s d <@ LSTemp

setFormId :: !(FormId d) !d -> FormId d					// set new initial value in formid
setFormId formid d		= reuseFormId formid d

reuseFormId :: !(FormId d) !v -> FormId v
reuseFormId formid v	= {formid & ival = v}

onMode :: !Mode a a a a -> a
onMode Edit    e1 e2 e3 e4 = e1
onMode Submit  e1 e2 e3 e4 = e2
onMode Display e1 e2 e3 e4 = e3
onMode NoForm  e1 e2 e3 e4 = e4

toViewId :: !Init !d !(Maybe d) -> d
toViewId Init d Nothing = d
toViewId Init d (Just v)= v
toViewId _  d _ 		= d

toViewMap :: !(d -> v) !Init !d !(Maybe v) -> v
toViewMap f init d mv	= toViewId init (f d) mv

derive gEq Mode, Init, Lifespan
instance == Mode        where == m1 m2 = m1 === m2
instance == Init        where == i1 i2 = i1 === i2
instance == Lifespan    where == l1 l2 = l1 === l2

instance < Lifespan     where (<) l1 l2 = toInt l1 < toInt l2

instance toBool Init    where toBool Set = True
							  toBool _   = False

instance toInt Lifespan where toInt LSTemp			= 0
							  toInt LSClient		= 1
							  toInt LSPage			= 2
							  toInt LSSession		= 3
							  toInt LSTxtFileRO		= 4
							  toInt LSTxtFile		= 5
							  toInt LSDataFile		= 6

instance toString Lifespan where 	
							  toString LSTemp		= "Temp"
							  toString LSClient		= "Client"
							  toString LSPage		= "Page"
							  toString LSSession	= "Session"
							  toString LSTxtFileRO	= "TxtFileRO"
							  toString LSTxtFile	= "TxtFile"
							  toString LSDataFile	= "DataFile"
instance toString StorageFormat where
							  toString PlainString	= "PlainString"
							  toString StaticDynamic = "StaticDynamic"
