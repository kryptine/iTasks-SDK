/**
* This panel will show the current work in tabs.
*/

Ext.ns("itasks");

itasks.WorkTabsPanel = Ext.extend(Ext.TabPanel, {
	
	initComponent: function() {
		Ext.apply(this,{
			activeItem: 0,
			layoutOnTabChange: true,
			items: {xtype: "itasks.hometab"}
		});
		
		this.addEvents("taskOpened");
		
		itasks.WorkTabsPanel.superclass.initComponent.apply(this,arguments);
	},
	openWorkTab: function (taskid) {

		//Id is prefixed with the string "worktab-"
		var id = "worktab-" + taskid;
		
		//Try to find an existing tab with the same id
		var tab = this.getComponent(id);
		var isnew = false;
		
		if(tab == undefined) {
			//Create new tab
			isnew = true;
			tab = new itasks.WorkPanel({id: id, taskId: taskid});
			
			//Add new tab
			this.add(tab);
			this.activate(tab);
			this.doLayout();
		}
		this.activate(tab);
		
		this.fireEvent("taskOpened",taskid);
				
		//Return a reference to the new tab
		return [tab,isnew];
	},
	openDebugTab: function() {
		var tab = this.getComponent("debugtab");
		if(tab == undefined) {
			tab = new itasks.DebugPanel({id: "debugtab", worktabs: this, closable: true});
			this.add(tab);
		}
		this.activate(tab);
		return tab;	
	},
	openTaskForestTab: function () {
		var tab = this.getComponent("taskforesttab");
		if(tab == undefined) {
			tab = new itasks.TaskForestTabPanel({id: "taskforesttab"});
			this.add(tab);
		}
		this.activate(tab);
		tab.refresh();
		return tab;
	},
	openProcessTableTab: function () {
		var tab = this.getComponent("processtabletab");
		if(tab == undefined) {
			tab = new itasks.ProcessTableTabPanel({id: "processtabletab"});
			this.add(tab);
		}
		this.activate(tab);
		tab.refresh();
		return tab;
	}
});

Ext.reg("itasks.worktabs",itasks.WorkTabsPanel);