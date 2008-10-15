Ext.onReady(function(){

	Ext.BLANK_IMAGE_URL = './ext/resources/images/defaults/s.gif';
	Ext.ns('itasks');

	app = new itasks.Application();
	app.start();
});
