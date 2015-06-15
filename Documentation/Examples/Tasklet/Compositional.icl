module Compositional

import iTasks
import iTasks.API.Core.Client.Editlet
import iTasks.API.Core.Client.Interface

import Data.Maybe

import StdArray, StdMisc

class ToEditlet sv ~ d cl | iTask sv
where
	toEditlet :: sv -> Editlet sv d cl

listEditlet :: (a,b) -> Editlet (a,b) (Maybe da, Maybe db) (ca,cb) | ToEditlet a da ca & ToEditlet b db cb
listEditlet (a,b)
  = { Editlet
    | currVal    = (a,b)
    
    , defValSrv  = (aeditlet.Editlet.defValSrv, beditlet.Editlet.defValSrv)
    
    , genUI      = genUI
    , initClient = initClient
    , appDiffClt = appDiffClt
    , genDiffSrv = genDiffSrv
    , appDiffSrv = appDiffSrv
    }
    
where
	aeditlet = toEditlet a
	beditlet = toEditlet b

	cida cid = cid +++ "_1"
	cidb cid = cid +++ "_2"

	initClient mkHandler cid world 
		# (cla, world) = aeditlet.Editlet.initClient (liftHandler_a mkHandler) (cida cid) world
		# (clb, world) = beditlet.Editlet.initClient (liftHandler_b mkHandler) (cidb cid) world
		= ((cla, clb), world)
	
	liftHandler_a mkHandler fun cid 
		= mkHandler wrapper cid
	where	
		wrapper cid eventObjs (cla,clb) world
			# (ncla, diff, world) = fun (cida cid) eventObjs cla world
			= ((ncla,clb), liftDiff diff, world)

		liftDiff NoDiff = NoDiff
		liftDiff (Diff d cont) = Diff (Just d, Nothing) (liftCont cont)

		liftCont cont conflict (cla, clb) world
			# (ncla, diff, world) = cont conflict cla world
			= ((ncla, clb), liftDiff diff, world)

	liftHandler_b mkHandler fun cid 
		= mkHandler wrapper cid
	where	
		wrapper cid eventObjs (cla, clb) world
			# (nclb, diff, world) = fun (cidb cid) eventObjs clb world
			= ((cla,nclb), liftDiff diff, world)

		liftDiff NoDiff = NoDiff
		liftDiff (Diff d cont) = Diff (Nothing, Just d) (liftCont cont)

		liftCont cont conflict (cla, clb) world
			# (nclb, diff, world) = cont conflict clb world
			= ((cla, nclb), liftDiff diff, world)

	genDiffSrv (cla1,clb1) (cla2,clb2)
		= case (aeditlet.Editlet.genDiffSrv cla1 cla2, beditlet.Editlet.genDiffSrv clb1 clb2) of
			(Nothing, Nothing) = Nothing
			diff			   = Just diff

	appDiffSrv (da, db) (a, b)
		= (maybe a (\d -> aeditlet.Editlet.appDiffSrv d a) da,
		   maybe b (\d -> beditlet.Editlet.appDiffSrv d b) db)

	appDiffClt mkHandler cid (da, db) (cla, clb) world
		# (cla, world) = case da of
			(Just d) = aeditlet.Editlet.appDiffClt (liftHandler_a mkHandler) (cida cid) d cla world
					 = (cla, world)
					 
		# (clb, world) = case db of
			(Just d) = beditlet.Editlet.appDiffClt (liftHandler_b mkHandler) (cidb cid) d clb world
					 = (clb, world)

		= ((cla, clb), world)

	genUI cid world
		# (ahtml, world) = aeditlet.Editlet.genUI cid world
		# (bhtml, world) = beditlet.Editlet.genUI cid world
		= ({ ComponentHTML
		   | html 			= DivTag [] [ahtml.ComponentHTML.html, bhtml.ComponentHTML.html]
		   , width 			= maxSize ahtml.ComponentHTML.width bhtml.ComponentHTML.width
		   , height			= maxSize ahtml.ComponentHTML.height bhtml.ComponentHTML.height
		   }, world)
		
	maxSize FlexSize _ = FlexSize   	
	maxSize _ FlexSize = FlexSize

	maxSize WrapSize _ = WrapSize   	
	maxSize _ WrapSize = WrapSize

	maxSize (ExactSize a) (ExactSize b) = ExactSize (max a b)   	

test = updateInformation "" [] 1

Start :: *World -> *World
Start world = startEngine test world


    


