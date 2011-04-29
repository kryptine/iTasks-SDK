Ext.ns('itasks.ttc');

itasks.ttc.MenuItem = Ext.extend(Ext.menu.Item, {

	initComponent: function() {
		
		this.listeners = {click: {fn: this.onActionClick, scope: this}};

		itasks.ttc.MenuItem.superclass.initComponent.apply(this,arguments);
		
		this.addEvents('tuiaction');
		this.enableBubble('tuiaction');
	},
	onActionClick: function() {
		if(this.action) {
			this.fireEvent('tuiaction',this.target,this.action);
		}
	}
});

Ext.reg('itasks.ttc.MenuItem',itasks.ttc.MenuItem);
