Ext.ns('itasks.tui');

itasks.tui.Button = Ext.extend(Ext.Button, {
	margins: {top:3, right:3, bottom:3, left:3},
	minWidth: 75,
	initComponent: function() {
		if (!this.isIconPresent(this.iconCls)) delete this.iconCls;
		this.listeners = {click: {fn: this.onActionClick, scope: this}};
		
		itasks.tui.Button.superclass.initComponent.apply(this,arguments);
		
		this.addEvents('tuiaction');
		this.enableBubble('tuiaction');
	},
	onActionClick: function() {
		this.fireEvent('tuiaction',this.taskId,this.name);
	},
	isIconPresent: function(cls) {
		for(x in Ext.util.CSS.getRules()) {
			if (x.indexOf(cls) != -1) return true;
		}
		
		return false;
	}
});

Ext.reg('itasks.tui.Button',itasks.tui.Button);