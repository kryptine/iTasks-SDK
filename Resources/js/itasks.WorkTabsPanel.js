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
	/**
	* Opens a new WorkTabPanel to work on a task
	*/	
	openWorkTab: function (taskid, taskinfo) {

		//Try to find an existing tab with the same id
		var tab = this.getComponent(taskid);

		if(tab == undefined) {
			//Create new tab
			tab = new itasks.WorkTabPanel({id: taskid,	taskinfo: taskinfo});
			//Add new tab
			this.add(tab);
			this.activate(tab);
			this.doLayout();
		}
		this.activate(tab);
				
		//Return a reference to the new tab
		return tab;
	},
	
	/**
	*	Opens the TaskForestTabPanel 
	*/
	openTaskForestTab: function (appPanel) {
		var tab = this.getComponent("taskforesttab");
		if(tab == undefined) {
			tab = new itasks.TaskForestTabPanel({id: "taskforesttab"});
			tab.setApplicationPanel(appPanel);
			this.add(tab);
			
		}
		this.activate(tab);
		tab.refresh();
		return tab;
	},
	/**
	* Opens the ThreadTableTabPanel
	*/
	openThreadTableTab: function (appPanel) {
		var tab = this.getComponent("threadtabletab");
		if(tab == undefined) {
			tab = new itasks.ThreadTableTabPanel({id: "threadtabletab"});
			tab.setApplicationPanel(appPanel);
			this.add(tab);
		}
		this.activate(tab);
		tab.refresh();
		return tab;
	},
	/**
	* Opens the ProcessTableTabPanel
	*/
	openProcessTableTab: function (appPanel) {
		var tab = this.getComponent("processtabletab");
		if(tab == undefined) {
			tab = new itasks.ProcessTableTabPanel({id: "processtabletab"});
			tab.setApplicationPanel(appPanel);
			this.add(tab);
		}
		this.activate(tab);
		tab.refresh();
		return tab;
	}
});

Ext.reg('itasks.worktabs',itasks.WorkTabsPanel);