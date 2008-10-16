/**
* This panel will show the current work in tabs.
*/

Ext.ns('itasks');

itasks.WorkTabsPanel = Ext.extend(Ext.TabPanel, {

	initComponent: function() {
		Ext.apply(this,{
			activeItem: 0,
			items: {xtype: 'itasks.hometab'}
		});
		
		itasks.WorkTabsPanel.superclass.initComponent.apply(this, arguments);
	}
});

Ext.reg('itasks.worktabs',itasks.WorkTabsPanel);