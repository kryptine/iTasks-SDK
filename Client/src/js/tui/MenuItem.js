Ext.ns('itasks.tui');

itasks.tui.MenuItem = Ext.extend(Ext.menu.Item, {

	initComponent: function() {
		
		this.listeners = {click: {fn: this.onActionClick, scope: this}};
		

		itasks.tui.MenuItem.superclass.initComponent.apply(this,arguments);
		
		this.addEvents('tuiaction');
		this.enableBubble('tuiaction');
	},
	onActionClick: function() {
		if(this.action) {
			this.fireEvent('tuiaction',this.target,this.action);
		}
	}
});

Ext.reg('itasks.tui.MenuItem',itasks.tui.MenuItem);
