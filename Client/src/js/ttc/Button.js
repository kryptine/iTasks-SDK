Ext.ns('itasks.ttc');

itasks.ttc.Button = Ext.extend(Ext.Button, {

	initComponent: function() {
	
		this.listeners = {click: {fn: this.onActionClick, scope: this}};
		
		itasks.ttc.Button.superclass.initComponent.apply(this,arguments);
		
		this.addEvents('tuiaction');
		this.enableBubble('tuiaction');
	},
	onActionClick: function() {
		if(this.action) {
			this.fireEvent('tuiaction',this.action);
		}
	}

});

Ext.reg('itasks.ttc.Button',itasks.ttc.Button);