Ext.define('itasks.container.ListItem',{
	extend: 'Ext.panel.Panel',
	alias: 'widget.itasks.list.item',
	requires: ['itasks.layout.VHBox'],
	layout: 'vhbox',

	initComponent: function() {	
		if(!this.width && !this.hwrap && !this.hflex) {
			this.hwrap = true;
			this.hflex = 1;
		}
		if(!this.height && !this.vwrap && !this.vflex) {
			this.vwrap = true;
		}
		this.addEvents('edit');
		this.enableBubble('edit');

		this.callParent(arguments);
	},
	onAdded: function() {
		this.callParent(arguments);
		if(this.taskId) {
			this.addDocked({
				xtype: 'panel',
				layout: 'card',
				width: 75,
				height: 22,
				dock: 'right',
				items: [{
					xtype: 'toolbar',
					height: 22,
					width: 75,
					items: [
						{xtype: 'button', iconCls: 'list-button-up', listeners: {click: {fn: this.handleItemUp, scope: this}}},
						{xtype: 'button', iconCls: 'list-button-down', listeners: {click: {fn: this.handleItemDown, scope: this}}},
						{xtype: 'button', iconCls: 'list-button-rem', listeners: {click: {fn: this.handleItemRemove, scope: this}}}
					]},
					{xtype: 'component', width: 75, height: 20, padding: 3, html: 'New...'}
					]
			});
		}
	},
	afterRender: function() {
		this.callParent(arguments);
	},
	//Based on the current position in the list, update the move/delete controls
	updateListTools: function() {
		var	me = this,
			ownerCt = me.ownerCt,
			listlen = ownerCt.items.length,
			listindex = me.index,
				
			isNewEntryItem = (listindex == listlen - 1),
			isTopItem = (listindex == 0),
			isBottomItem = (listindex == listlen - 2),
			dock, toolbar;

		if(ownerCt && ownerCt.taskId) {
			dock = me.getDockedComponent(0);
			if(dock) {
				if(isNewEntryItem) {	
					dock.getLayout().setActiveItem(1);
				} else {
					dock.getLayout().setActiveItem(0);
					toolbar = dock.getComponent(0);
					toolbar.getComponent(0)[isTopItem ? 'disable' : 'enable']();
					toolbar.getComponent(1)[isBottomItem ? 'disable' : 'enable']();
				}
			}
		}
	},
	handleItemRemove: function() {
		this.fireEvent('edit',this.taskId, this.name, 'rem_' + this.index);
	},
	handleItemUp: function() {
		this.fireEvent('edit',this.taskId, this.name, 'mup_' + this.index);
	},
	handleItemDown: function() {
		this.fireEvent('edit',this.taskId, this.name, 'mdn_' + this.index);
	}
});
