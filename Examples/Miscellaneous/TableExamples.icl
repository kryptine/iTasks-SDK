implementation module TableExamples

import iTasks, Table, Text, StdInt

tableExamples :: [Workflow]
tableExamples = [workflow "Examples/Miscellaneous/Plant dataset table" "Uses the Table type to represent a simple plant dataset." plantExample]

plantExample = try plantExample` showError

plantExample` =
		readDataset
	>>= transform toTable
	>>=	updateInformation ("Plant Dataset",description)
	>>|	stop
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
	>>= \csvData.	toPlant csvData []
where
	toPlant [] acc = return (reverse acc)
	toPlant [plant:rest] acc = case plant of
		[common,botanical,zone,light,price,availability,indoor]
			= toPlant rest
				[{ name =
					{ common	= common
					, botanical	= botanical
					}
				, light			= light
				, price			= USD (toInt ((toReal price) * 100.0))
				, availability	= s2Date availability
				,indoor			= indoor == "true"
				}:acc]
		_
			= throw "invalid CSV row!"

	s2Date str = case split "/" str of
		[m,d,y]	= {day = toInt d, mon = toInt m, year = toInt y}
		_		= {day = 0, mon = 0, year = 0}

:: Plant =		{ name			:: PlantName
				, light			:: String
				, price			:: Currency
				, availability	:: Date
				, indoor		:: Bool
				}
:: PlantName =	{ common		:: String
				, botanical		:: String
				}

derive class iTask		Plant, PlantName
derive class tableRow	Plant, PlantName
derive bimap Maybe, (,)
