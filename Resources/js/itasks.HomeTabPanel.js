/**
* Tab panel which shows a dashboard style 'home' screen.
*/

Ext.ns('itasks');

itasks.HomeTabPanel = Ext.extend(Ext.Panel, {

	initComponent: function () {
		Ext.apply(this, {
			title: 'Home',
			closable: false,
			html: 'This panel will show a dashboard style homepage',
			bodyStyle: 'padding: 5px;'
		});
		
		itasks.HomeTabPanel.superclass.initComponent.apply(this, arguments);
	}
});

Ext.reg('itasks.hometab',itasks.HomeTabPanel);