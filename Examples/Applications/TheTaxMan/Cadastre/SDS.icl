implementation module Cadastre.SDS

import Cadastre.UoD

cadastreRealEstate :: SimpleSDSLens [CadastreRealEstate]
cadastreRealEstate = sharedStore "cadastreRealEstate" []
