/*
    This file is generated and updated by Sencha Cmd. You can edit this file as
    needed for your application, but these edits will have to be merged by
    Sencha Cmd when it performs code generation tasks such as generating new
    models, controllers or views and when running "sencha app upgrade".

    Ideally changes to this file would be limited and most work would be done
    in other places (such as Controllers). If Sencha Cmd cannot merge your
    changes and its generated code, it will produce a "merge conflict" that you
    will need to resolve manually.
*/

// DO NOT DELETE - this directive is required for Sencha Cmd packages to work.
//@require @packageOverrides

 Ext.Loader.setConfig({
     enabled : true //enable the dynamic dependency loading feature
     ,disableCaching : false //Comment this line out when you need to reload your Extjs js files
 });


Ext.application({
	name: 'itwc', //iTasks Web Client
	
	//Apply patches
	requires: ['itwc.patch.grid.View'
			  ,'itwc.patch.layout.Context'
			  ,'itwc.patch.AbstractComponent'
			  ],

	//Core controller that syncs with server-side session state
	controllers: ['Controller'],

	//Core container that holds all other components	
	views: ['Viewport'],
	autoCreateViewport: true
});

