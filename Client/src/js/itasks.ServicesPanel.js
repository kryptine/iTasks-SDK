/**
* Panel which opens the services url for debugging.
*/
Ext.ns("itasks");

itasks.ServicesPanel = Ext.extend(Ext.Panel, {

	initComponent: function () {
		Ext.apply(this, {
			title: "Services",
			closable: true,
			iconCls: "icon-services",
			html: '<iframe style="overflow:auto;width:100%;height:100%;" frameborder="0" src="' + itasks.config.serviceUrl + '/html' + '"></iframe>'
		});
		
		itasks.ServicesPanel.superclass.initComponent.apply(this, arguments);
	}
});

Ext.reg("itasks.services",itasks.ServicesPanel);