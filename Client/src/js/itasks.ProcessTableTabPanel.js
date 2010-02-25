/**
* Tab panel which shows the global process table.
*/

Ext.ns('itasks');

itasks.ProcessTableTabPanel = Ext.extend(itasks.RemoteDataPanel, {

	applicationPanel: undefined,
	
	initComponent: function () {
		Ext.apply(this, {
			title: "Process table",
			closable: true,
			autoScroll: true,
			url: itasks.config.serverUrl + "/debug/processtable",
			cls: "worktab-container",
			iconCls: "icon-process-table",
			bodyStyle: "padding: 10px;",
			tbar: [{
					text: "Refresh",
					iconCls: "x-tbar-loading",
					listeners: { click: { fn: function (btn) {this.refresh();}, scope: this}}
				}]
		});	
		itasks.ProcessTableTabPanel.superclass.initComponent.apply(this, arguments);
	},
	update: function(data) {
		this.body.dom.innerHTML = data;
	}
});

Ext.reg("itasks.processtabletab",itasks.ProcessTableTabPanel);