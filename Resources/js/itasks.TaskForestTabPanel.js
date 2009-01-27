/**
* Tab panel which shows the global task forest.
*/

Ext.ns('itasks');

itasks.TaskForestTabPanel = Ext.extend(Ext.Panel, {

	applicationPanel: undefined,
	
	initComponent: function () {
		Ext.apply(this, {
			title: 'Task forest',
			closable: true,
			autoScroll: true,
			cls: 'worktab-container',
			tbar: [{
					text: 'Refresh',
					iconCls: 'x-tbar-loading',
					listeners: {
						click: {
							scope: this,
							fn: function (btn) {
								this.refresh();
							}
						}
					}
				}]
		});
		
		itasks.TaskForestTabPanel.superclass.initComponent.apply(this, arguments);
	},
	setApplicationPanel: function (panel) {
		this.applicationPanel = panel;
	},
	processResponse: function (el, success, response, options) {
		if(response.responseText.substr(0,4) == '<div') {
			this.body.dom.innerHTML = response.responseText;
		} else {
			this.applicationPanel.checkSessionResponse(Ext.decode(response.responseText));
		}
	},
	refresh: function() {
		Ext.Ajax.request({
			method: 'POST',
			url: 'handlers/debug/tasktreeforest',
			params: this.applicationPanel.addSessionParam({}),
			callback: this.processResponse,
			scope: this
		});
	}
});

Ext.reg('itasks.taskforesttab',itasks.TaskForestTabPanel);