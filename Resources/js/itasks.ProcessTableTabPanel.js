/**
* Tab panel which shows the global process table.
*/

Ext.ns('itasks');

itasks.ProcessTableTabPanel = Ext.extend(Ext.Panel, {

	applicationPanel: undefined,
	
	initComponent: function () {
		Ext.apply(this, {
			title: 'Process table',
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
		
		itasks.ProcessTableTabPanel.superclass.initComponent.apply(this, arguments);
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
			url: 'handlers/processtable',
			params: this.applicationPanel.addSessionParam({}),
			callback: this.processResponse,
			scope: this
		});
	}
});

Ext.reg('itasks.processtabletab',itasks.ProcessTableTabPanel);