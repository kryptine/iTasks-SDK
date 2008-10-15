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
					xtype: 'treepanel',
					region: 'west',
					split: true,
					title: 'Current work',
					border: false,
					width: 200,
					minWidth: 100,
					maxWidth: 400,
					dataUrl: 'handlers/filters',
					root: { text: 'All work', nodeType: 'async', id: 'all' }
				},{
					region: 'center',
					xtype: 'panel',
					layout: 'border',
					border: false,
					items: [ {
						region: 'north',
						split: true,
						height: 150,
						html: 'TASK LIST'
					},{
						xtype: 'tabpanel',
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