definition module Cadastre.SDS

import Cadastre.UoD

/** cadastreRealEstate:
		this shared data source keeps track of the registered owners per address.
*/
cadastreRealEstate :: SDSLens () [CadastreRealEstate] [CadastreRealEstate]
