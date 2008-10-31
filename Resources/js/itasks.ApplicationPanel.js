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
					id: 'centerpanel',
					region: 'center',
					xtype: 'panel',
					layout: 'border',
					border: false,
					items: [ {
						id: 'worklist',
						xtype: 'itasks.worklist',
						region: 'north',
						split: true,
						height: 150
					},{
						id: 'worktabs',
						xtype: 'itasks.worktabs',
						border: false,
						region: 'center'
					}],
					tbar: [
						{ id: 'refreshbutton'
						, text: 'Refresh worklist'
						, iconCls: 'icon-refresh'
						, listeners: {
								click : { scope: this, fn: function (btn) {
									this.getComponent('centerpanel').getComponent('worklist').refresh();
								}}
							}
						}
					]
				}]
		});
	
		itasks.ApplicationPanel.superclass.initComponent.apply(this, arguments);
	},
	init: function () {
		//Initializing the gui...
		var worklist = this.getComponent('centerpanel').getComponent('worklist');
		var worktabs = this.getComponent('centerpanel').getComponent('worktabs');
		
		//Connect event handlers	
		worklist.on('cellclick',function (grid,row,col,event) {
			worktabs.openWorkTab(grid.getTaskId(row));
		});
	}
});