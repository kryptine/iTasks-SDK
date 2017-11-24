implementation module Cadastre.Tasks

import Task.Extensions
import Cadastre.SDS

editCadastreRealEstate :: Task ()
editCadastreRealEstate = editStore "List of cadastre real estate" cadastreRealEstate
