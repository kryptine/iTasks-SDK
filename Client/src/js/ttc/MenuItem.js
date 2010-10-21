Ext.ns('itasks.ttc');

itasks.ttc.MenuItem = Ext.extend(Ext.menu.Item, {

	initComponent: function() {
		
		this.listeners = {click: {fn: this.onActionClick, scope: this}};

		itasks.ttc.MenuItem.superclass.initComponent.apply(this,arguments);
		
		this.addEvents('tuievent');
		this.enableBubble('tuievent');
	},
	onActionClick: function() {
		if(this.action) {
			this.fireEvent('tuievent',this.target,'action',[this.action,'MENU']);
		}
	}
});

Ext.reg('itasks.ttc.MenuItem',itasks.ttc.MenuItem);
