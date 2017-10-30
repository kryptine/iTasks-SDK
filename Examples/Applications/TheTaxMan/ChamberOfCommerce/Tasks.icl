implementation module ChamberOfCommerce.Tasks

import Task.Extensions
import ChamberOfCommerce.SDS

editCompanies :: Task ()
editCompanies = editStore "List of companies" companies
