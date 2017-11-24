implementation module CivilAffairs.Tasks

import Task.Extensions
import CivilAffairs.SDS

editCitizens :: Task ()
editCitizens = editStore "List of citizens" citizens
