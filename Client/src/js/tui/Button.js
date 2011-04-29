Ext.ns('itasks.tui');

itasks.tui.Button = Ext.extend(Ext.Button, {
	minWidth: 75,
	initComponent: function() {
		if (!this.isIconPresent(this.iconCls)) delete this.iconCls;
		this.listeners = {click: {fn: this.onActionClick, scope: this}};
		delete this.width;
		
		itasks.tui.Button.superclass.initComponent.apply(this,arguments);
		
		if (this.actionButton) {
			this.addEvents('tuiaction');
			this.enableBubble('tuiaction');
		} else {
			this.addEvents('tuichange');
			this.enableBubble('tuichange');
		}
	},
	onActionClick: function() {
		if (this.actionButton) {
			this.fireEvent('tuiaction',this.taskId,this.name);
		} else {
			this.fireEvent('tuichange',this.taskId,this.name,null);
		}
	},
	isIconPresent: function(cls) {
		for(x in Ext.util.CSS.getRules()) {
			if (x.indexOf(cls) != -1) return true;
		}
		
		return false;
	}
});

Ext.reg('itasks.tui.Button',itasks.tui.Button);