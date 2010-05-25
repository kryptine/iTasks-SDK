Ext.ns("itasks");

Ext.QuickTips.init();

Ext.onReady(function(){
	itasks.app = new itasks.Application();
	itasks.app.start();
});
