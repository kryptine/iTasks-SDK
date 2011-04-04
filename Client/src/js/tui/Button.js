Ext.ns('itasks.tui');

itasks.tui.Button = Ext.extend(Ext.Button, {
	margins: {top:3, right:3, bottom:3, left:3},
	minWidth: 75,
	initComponent: function() {
	
		this.listeners = {click: {fn: this.onActionClick, scope: this}};
		
		itasks.tui.Button.superclass.initComponent.apply(this,arguments);
		
		this.addEvents('tuiaction');
		this.enableBubble('tuiaction');
	},
	onActionClick: function() {
		if(this.action) {
			this.fireEvent('tuiaction',this.action);
		}
	}

});

Ext.reg('itasks.tui.Button',itasks.tui.Button);