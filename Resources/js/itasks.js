Ext.onReady(function(){

	Ext.BLANK_IMAGE_URL = './ext/resources/images/defaults/s.gif';
	Ext.ns('itasks');

	lw = new itasks.LoginWindow();
	lw.startApplication = function(uid, sessionKey) {

		ld = new itasks.LoaderWindow();
		ld.show();

//		ld.updateProgress(0.5,'Loading SAPL applet...');

//		console.log(uid);
//		console.log(sessionKey);
	}
	lw.show();
});
