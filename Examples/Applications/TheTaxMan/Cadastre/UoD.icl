implementation module Cadastre.UoD

import CivilAffairs.UoD, ChamberOfCommerce.UoD
import Data.Either

derive class iTask CadastreRealEstate
instance == CadastreRealEstate where (==) a1 a2 = a1.CadastreRealEstate.address == a2.CadastreRealEstate.address
instance <  CadastreRealEstate where (<)  a1 a2 = a1.CadastreRealEstate.address <  a2.CadastreRealEstate.address
