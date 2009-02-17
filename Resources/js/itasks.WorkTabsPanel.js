/**
* This panel will show the current work in tabs.
*/

Ext.ns('itasks');

itasks.WorkTabsPanel = Ext.extend(Ext.TabPanel, {

	sessionId: undefined,
	application: undefined,
	
	initComponent: function() {
		Ext.apply(this,{
			activeItem: 0,
			items: {xtype: 'itasks.hometab'}
		});
		
		itasks.WorkTabsPanel.superclass.initComponent.apply(this, arguments);
	},
	openWorkTab: function (taskid, taskinfo) {

		//Id is prefixed with the string "worktab-"
		var id = "worktab-" + taskid;
		
		//Try to find an existing tab with the same id
		var tab = this.getComponent(id);
		var isnew = false;
		
		if(tab == undefined) {
			//Create new tab
			isnew = true;
			tab = new itasks.WorkTabPanel({id: id,	taskinfo: taskinfo, sessionId: this.sessionId, application: this.application});
			//Add new tab
			this.add(tab);
			this.activate(tab);
			this.doLayout();
		}
		this.activate(tab);
				
		//Return a reference to the new tab
		return [tab,isnew];
	},
	openTaskForestTab: function () {
		var tab = this.getComponent("taskforesttab");
		if(tab == undefined) {
			tab = new itasks.TaskForestTabPanel({id: "taskforesttab", sessionId: this.sessionId, application: this.application});
			this.add(tab);
		}
		this.activate(tab);
		tab.refresh();
		return tab;
	},
	openProcessTableTab: function () {
		var tab = this.getComponent("processtabletab");
		if(tab == undefined) {
			tab = new itasks.ProcessTableTabPanel({id: "processtabletab", sessionId: this.sessionId, application: this.application});
			this.add(tab);
		}
		this.activate(tab);
		tab.refresh();
		return tab;
	}
});

Ext.reg('itasks.worktabs',itasks.WorkTabsPanel);