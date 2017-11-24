implementation module Cadastre.UoD

import CivilAffairs.UoD, ChamberOfCommerce.UoD
from Data.Either import :: Either(..)

derive class iTask CadastreRealEstate
instance == CadastreRealEstate where == a1 a2 = a1.CadastreRealEstate.address == a2.CadastreRealEstate.address
instance <  CadastreRealEstate where <  a1 a2 = a1.CadastreRealEstate.address <  a2.CadastreRealEstate.address
instance == (Either a b) | Eq  a & Eq  b where == (Left  a) (Left  b) = a == b
                                               == (Right a) (Right b) = a == b
                                               == _         _         = False 
instance <  (Either a b) | Ord a & Ord b where <  (Left  a) (Left  b) = a < b
                                               <  (Left  _) _         = True
                                               <  (Right a) (Right b) = a < b
                                               <  _         _         = False
