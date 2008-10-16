/**
* Main iTasks user interface
*/
Ext.ns('itasks');

itasks.ApplicationPanel = Ext.extend(Ext.Panel, {

	initComponent: function() {
		Ext.apply(this, {
			layout: 'border',
			hidden: true,
			items: [{
					xtype: 'panel',
					region: 'north',
					baseCls: 'bg',
					height: 75
				},{
					xtype: 'panel',
					region: 'west',
					layout: 'accordion',
					layoutConfig: {animate: true},
					split: true,
					border: false,
					width: 200,
					minWidth: 100,
					maxWidth: 400,
					items: [
						{xtype: 'itasks.cwpanel' },
						{xtype: 'itasks.nwpanel' },
						{xtype: 'itasks.debug' }
					]
				},{
					region: 'center',
					xtype: 'panel',
					layout: 'border',
					border: false,
					items: [ {
						xtype: 'itasks.worklist',
						region: 'north',
						split: true,
						height: 150
					},{
						xtype: 'itasks.worktabs',
						border: false,
						region: 'center'
					}]
				}]
		});
	
		itasks.ApplicationPanel.superclass.initComponent.apply(this, arguments);
	},
	init: function () {
		//Initializing the gui...
	}
});