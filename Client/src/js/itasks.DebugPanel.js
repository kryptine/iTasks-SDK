/**
* Panel for debugging iTask workflows. Here we will put stuff such as
* task tree visualization and communication state inspection.
*/
Ext.namespace("itasks");

itasks.DebugPanel = Ext.extend(Ext.Panel, {

	worktabs: null,
	initComponent: function () {
		Ext.apply(this, {	
			title: "Debug",
			iconCls: "icon-debug",
			bodyStyle: "padding: 5px",
			deferredRender: false,
			items: [{
				xtype: "fieldset",
				title: "Overviews",
				autoHeight: true,
				items: [{
					xtype: "button",
					text: "Browse service API...",
					iconCls: "icon-services",
					style: "margin-bottom: 2px;",
					listeners: {
						click: {
							fn: function() {
								this.worktabs.openServicesTab();
							},
							scope: this	
						}
					}
				}]
			},{
				xtype: "fieldset",
				title: "Session",
				html: itasks.app.session,
				autoHeight: true,
				hideLabels: true
			}]
		});
		
		itasks.DebugPanel.superclass.initComponent.apply(this,arguments);
	}
});

Ext.reg("itasks.debug",itasks.DebugPanel);


