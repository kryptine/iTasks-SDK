Ext.ns('itasks.tui');

itasks.tui.Button = itasks.util.extend(Ext.Button, itasks.tui.base, {
	height: 25,
	hwrap: true,
	minWidth: 75,
	initComponent: function() {
		if (!this.isIconPresent(this.iconCls)) delete this.iconCls;
		this.listeners = {click: {fn: this.onActionClick, scope: this}};
		
		if (!this.margins) {
			this.margins =	{ left		: 3
							, top		: 3
							, right		: 3
							, bottom	: 3
							};
		}
		
		itasks.tui.base.initComponent.apply(this,arguments);
		
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