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
				xtype: 'fieldset',
				title: 'Tracing',
				defaultType: 'checkbox',
				hideLabels: true,
				autoHeight: true,
				layout: 'form',
				items: [{
					name: 'traceStates',
					boxLabel: 'State information'
					},{
					name: 'traceUpdates',
					boxLabel: 'Form updates'
					},{
					name: 'traceSubTrees',
					boxLabel: 'Task sub-trees'
					}]
			}]
		});
		
		itasks.DebugPanel.superclass.initComponent.apply(this,arguments);
	}
});

Ext.reg('itasks.debug',itasks.DebugPanel);