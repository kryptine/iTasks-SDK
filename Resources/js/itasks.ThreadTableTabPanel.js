/**
* Tab panel which shows the global thread table.
*/

Ext.ns('itasks');

itasks.ThreadTableTabPanel = Ext.extend(Ext.Panel, {

	applicationPanel: undefined,
	
	initComponent: function () {
		Ext.apply(this, {
			title: 'Thread table',
			closable: true,
			autoScroll: true,
			cls: 'worktab-container'
		});
		
		itasks.ThreadTableTabPanel.superclass.initComponent.apply(this, arguments);
	},
	setApplicationPanel: function(panel) {
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
			method: 'GET',
			url: 'handlers/threadtable',
			params: this.applicationPanel.addSessionParam({}),
			callback: this.processResponse,
			scope: this
		});
	}
});

Ext.reg('itasks.threadtabletab',itasks.ThreadTableTabPanel);