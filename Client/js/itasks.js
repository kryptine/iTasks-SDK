Ext.BLANK_IMAGE_URL = "/ext3/resources/images/default/s.gif";

Ext.ns("itasks");

Ext.onReady(function(){
	itasks.app = new itasks.Application();
	itasks.app.start();
});
