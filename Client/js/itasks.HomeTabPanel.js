/**
* Tab panel which shows a dashboard style 'home' screen.
*/

Ext.ns("itasks");

itasks.HomeTabPanel = Ext.extend(Ext.Panel, {

	initComponent: function () {
		Ext.apply(this, {
			title: "Welcome",
			closable: false,
			autoLoad: "skins/" + itasks.config.skin + "/welcome.html"
		});
		
		itasks.HomeTabPanel.superclass.initComponent.apply(this, arguments);
	}
});

Ext.reg("itasks.hometab",itasks.HomeTabPanel);