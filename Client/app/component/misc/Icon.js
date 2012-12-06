Ext.define('itwc.component.misc.Icon', {
	extend: 'Ext.Component',
	alias: 'widget.itwc_icon',
	width: 16,
	height: 16,
	iconCls: 'icon-help',
	initComponent: function() {
		this.addCls(this.iconCls);
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
	},
	setType: function(iconCls) {
		this.removeCls(this.iconCls);
		this.iconCls = iconCls;
		this.addCls(this.iconCls);
	},
	setIconCls: function(iconCls) {
		this.setType(iconCls);
	},
	setTooltip: function(text) {
		this.tooltip = new Ext.ToolTip({html: text, target: this.getEl()});
	}
});
