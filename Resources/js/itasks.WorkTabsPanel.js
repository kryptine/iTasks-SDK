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
	},
	
	openWorkTab: function (taskid) {
	
		//Try to find an existing tab with the same id
		var tab = this.getComponent(taskid);
		
		if(tab == undefined) {
			//Create new tab
			tab = new itasks.WorkTabPanel({
				id: taskid
			});
			
			//Add new tab
			this.add(tab);
		}
		//Activate tab
		this.activate(tab);
	}
});

Ext.reg('itasks.worktabs',itasks.WorkTabsPanel);