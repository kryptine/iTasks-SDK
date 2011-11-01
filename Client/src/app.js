Ext.application({
	name: 'itasks',
	appFolder: '/src/app',
	requires: [
		'itasks.layout.VHBox',
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
