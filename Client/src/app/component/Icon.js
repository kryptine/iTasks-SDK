Ext.define('itasks.component.Icon', {
	extend: 'Ext.Component',
	alias: 'widget.itasks.icon',
	width: 18,
	height: 18,
	initComponent: function() {
		this.addCls(this.type || 'x-hint-icon');
		this.callParent(arguments);

	},
	afterRender: function() {
		this.callParent(arguments);
		if (this.tooltip) {
            		this.tooltip = new Ext.ToolTip({html: this.tooltip, target: this.getEl()});
        	}
	},
	onDestroy: function() {
		if (this.tooltip) {
			this.tooltip.destroy();
		}
		this.callParent(arguments);
	}
});
