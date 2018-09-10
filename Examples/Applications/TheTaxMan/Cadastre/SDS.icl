implementation module Cadastre.SDS

import Cadastre.UoD

cadastreRealEstate :: SDSLens () [CadastreRealEstate] [CadastreRealEstate]
cadastreRealEstate = sharedStore "cadastreRealEstate" []
