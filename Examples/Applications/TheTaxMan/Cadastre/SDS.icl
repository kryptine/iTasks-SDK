implementation module Cadastre.SDS

import Cadastre.UoD

cadastreRealEstate :: Shared [CadastreRealEstate]
cadastreRealEstate = sharedStore "cadastreRealEstate" []
