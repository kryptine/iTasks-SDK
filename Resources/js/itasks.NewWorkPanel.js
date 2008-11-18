/**
* Panel for starting new work, used to initiate new work flows
*/
Ext.ns('itasks');

itasks.NewWorkPanel = Ext.extend(Ext.Panel, {

	initComponent: function() {
		Ext.apply(this, {
			title: 'Start new work',
			iconCls: 'icon-newwork',
			html: 'This panel will be used to start new work',
			bodyStyle: 'padding: 5px'
		});
		
		itasks.NewWorkPanel.superclass.initComponent.apply(this,arguments);
	}
});

Ext.reg('itasks.nwpanel', itasks.NewWorkPanel);