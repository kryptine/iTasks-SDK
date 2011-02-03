implementation module TableExamples

import iTasks, Text, StdInt

tableExamples :: [Workflow]
tableExamples = [workflow "Plant dataset table" "Uses the Table type to represent a simple plant dataset." plantExample]

plantExample = try plantExample` showError

plantExample` =
		readDataset
	>>= transform Table
	>>=	updateInformation ("Plant Dataset",description)
	>>= showMessageAbout "Updated dataset"
	>>| stop
where
	description = RawText
		("This example demonstrates how the table type is used to edit a dataset. "
		+++
		"The dataset is taken from an <a href=http://dev.sencha.com/deploy/dev/examples/grid/edit-grid.html>Ext JS Grid Example</a>.")

showError dyn = case dyn of
	(str :: String)	= showMessage ("Error",str) Void
	_				= showMessage "Unknown error" Void

readDataset :: Task [Plant]
readDataset =
					importCSVFile ".\\Miscellaneous\\plants.csv"
	>>= \csvData.	toPlants csvData []
where
	toPlants [] acc = return (reverse acc)
	toPlants [plant:rest] acc = case plant of
		[common,botanical,zone,light,price,availability,indoor]
			= toPlants rest
				[{ name =
					{ common		= common
					, botanical		= botanical
					}
				, light 			= case light of
					"Sunny"			= Sunny
					"Sun or Shade"	= SunOrShade
					"Mostly Shady"	= MostlyShady
					_				= Shade
				, price				= USD (toInt (toReal price) * 100)
				, availability		= s2Date availability
				, indoor			= indoor == "true"
				, description		= Note ""
				}:acc]
		_
			= throw "invalid CSV row!"

	s2Date str = case split "/" str of
		[m,d,y]	= {day = toInt d, mon = toInt m, year = toInt y}
		_		= {day = 0, mon = 0, year = 0}

:: Plant =		{ name			:: PlantName
				, light			:: PlantLight
				, price			:: Currency
				, availability	:: Date
				, indoor		:: Bool
				, description	:: Note
				}
:: PlantName =	{ common		:: String
				, botanical		:: String
				}
:: PlantLight	= Sunny
				| SunOrShade
				| MostlyShady
				| Shade

derive class iTask Plant, PlantName, PlantLight
derive bimap Maybe, (,)
