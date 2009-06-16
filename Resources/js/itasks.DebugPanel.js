/**
* Panel for debugging iTask workflows. Here we will put stuff such as
* task tree visualization and communication state inspection.
*/
Ext.namespace('itasks');

itasks.DebugPanel = Ext.extend(Ext.Panel, {

	initComponent: function () {
		Ext.apply(this, {	
			title: 'Debug',
			iconCls: 'icon-debug',
			bodyStyle: 'padding: 5px',
			deferredRender: false,
			items: [{
				xtype: 'fieldset',
				title: 'Overviews',
				autoHeight: true,
				items: [{
					xtype: 'button',
					text: 'Show task forest...',
					cls: 'debug-button',
					iconCls: 'icon-task-tree',
					style: 'margin-bottom: 2px;'
				},{
					xtype: 'button',
					text: 'Show process table...',
					cls: 'debug-button',
					iconCls: 'icon-process-table'
				}]
			},{
				id: 'options',
				xtype: 'fieldset',
				title: 'Options',
				defaultType: 'checkbox',
				hideLabels: true,
				autoHeight: true,
				layout: 'form',
				items: [{
					id: 'trace',
					boxLabel: 'Enable trace'
					}]
			},{
				xtype: 'fieldset',
				title: 'Session Id',
				html: this.sessionId,
				autoHeight: true,
				hideLabels: true
			}]
		});
		
		itasks.DebugPanel.superclass.initComponent.apply(this,arguments);
	},
	getTaskForestButton: function() {
		return this.getComponent(0).getComponent(0);
	},
	getProcessTableButton: function() {
		return this.getComponent(0).getComponent(1);
	},
	getTraceCheckbox: function() {
		return this.getComponent(1).getComponent(0);
	}
});

Ext.reg('itasks.debug',itasks.DebugPanel);