/**
* Panel for debugging iTask workflows. Here we will put stuff such as
* task tree visualization and communication state inspection.
*/

Ext.namespace('itasks');

itasks.DebugPanel = Ext.extend(Ext.form.FormPanel, {

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
					minWidth: '160',
					ctCls: 'debug-button',
					iconCls: 'icon-task-tree',
					style: 'margin-bottom: 2px'
				},{
					xtype: 'button',
					text: 'Show thread table...',
					minWidth: '160',
					ctCls: 'debug-button',
					iconCls: 'icon-thread-table',
					style: 'margin-bottom: 2px'
				},{
					xtype: 'button',
					text: 'Show process table...',
					minWidth: '160',
					ctCls: 'debug-button',
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
			}]
		});
		
		itasks.DebugPanel.superclass.initComponent.apply(this,arguments);
	},
	traceEnabled: function() {
		return this.getComponent('options').getComponent('trace').getValue();
	},
	getTaskForestButton: function() {
		return this.getComponent(0).getComponent(0);
	},
	getThreadTableButton: function() {
		return this.getComponent(0).getComponent(1);
	},
	getProcessTableButton: function() {
		return this.getComponent(0).getComponent(2);
	}	
});

Ext.reg('itasks.debug',itasks.DebugPanel);