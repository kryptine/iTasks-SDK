Ext.application({
	name: 'itasks',
	appFolder: 'app',
	requires: [
		'itasks.layout.VHBox',
		'Ext.container.Viewport',
		//Extension classes
		'itasks.extension.GoogleMap'
	],
	controllers: [
		'Controller'
	],
	launch: function() {
		Ext.tip.QuickTipManager.init();
		
		Ext.create('Ext.container.Viewport', {
			layout: {
				type: 'vhbox',
				direction: 'vertical',
				valign: 'top',
				halign: 'center'	
			}
		});
	}
});
