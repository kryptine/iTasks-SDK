definition module StdiData

// top level module for generating Clean Web applications using the iData technique
// any of these modules can directly be used by the application programmer
// (c) 2005 - 2006 Rinus Plasmeijer

import

// iData modules:

			iDataSettings		// some global settings
		,	iDataFormData		// general iData type definitions
		,	iDataHandler		// *the* kernel module for iData creation and handling

		,	iDataButtons		// basic collections of buttons, data types for lay-out	control	
		,	iDataFormlib		// collection of advanced iData creating functions  
		,	iDataDatabase		// collection for storing data, while guarding consistency and versions
		,	iDataExceptions		// collection of global exception handling and storage
		,	iDataRefFormlib		// collection of persistent idata maintaining sharing

		,	iDataArrow			// arrow instantiations for iData forms

// html code generation:

	 	,	iDataHtmlDef		// Clean's ADT representation of Html
		,	iDataStyleDef		// Clean's ADT representation of Style sheets

// free to change when the default style of the generated web pages is not appealing:

		,	iDataStylelib		// style definitions used by iData  

// automatic data base storage and retrieval


// of general use:

		,	iDataTrivial		// some trivial generic bimap derives
		,	StdBimap			// some trival derives for bimap