Ext.define('itasks.container.ListItem',{
	extend: 'Ext.panel.Panel',
	alias: 'widget.ilisti',
	mixins: {
		wrappable: 'itasks.mixin.Wrappable'
	},
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
		
		this.addDocked([{
			xtype: 'toolbar',
			width: 75,
			layout: {type:'hbox', pack: 'align'},
			dock: 'right',
			items: [
				{xtype: 'component', html: '<i>New item</i>', hidden: true},
				{xtype: 'button', iconCls: 'list-button-up', listeners: {click: {fn: this.handleItemUp, scope: this}}},
				{xtype: 'button', iconCls: 'list-button-down', listeners: {click: {fn: this.handleItemDown, scope: this}}},
				{xtype: 'button', iconCls: 'list-button-rem', listeners: {click: {fn: this.handleItemRemove, scope: this}}}
			]
		},{
			xtype: 'component',
			dock: 'top',
			height: 5,
			hidden: true
		}]);
	},
	updateControls: function() {
		var me = this,
			container = me.up('ilistc'),
			listlength = container.items.length,
			listindex = me.index,
			toolbar = me.getDockedComponent(0),
			spacer = me.getDockedComponent(1),
			isFirst = (listlength > 1 && listindex == 0)
			isLast = (listindex == listlength - 2),
			isNew = (listindex == listlength - 1),
			i = 0;
			
		if(isNew) {	
			spacer.show();
			toolbar.getComponent(0).show();
			for(i = 1; i < 4; i++) {
				toolbar.getComponent(i).hide();
			}
		} else {
			spacer.hide();
			toolbar.getComponent(0).hide()
			for(i = 1; i < 4; i++) {
				toolbar.getComponent(i).show();
			}
			toolbar.getComponent(1)[isFirst ? 'disable' : 'enable']();
			toolbar.getComponent(2)[isLast ? 'disable' : 'enable']();
		}
	},
	handleItemRemove: function() {
		var list = this.up('ilistc');
		this.fireEvent('edit',list.taskId, list.name, 'rem_' + this.index);
	},
	handleItemUp: function() {
		var list = this.up('ilistc');
		this.fireEvent('edit',list.taskId, list.name, 'mup_' + this.index);
	},
	handleItemDown: function() {
		var list = this.up('ilistc');
		this.fireEvent('edit',list.taskId, list.name, 'mdn_' + this.index);
	}
});