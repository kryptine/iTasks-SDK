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
        var me = this;
		me.callParent(arguments);

		if (me.tooltip) {
            me.tooltip = Ext.create('Ext.tip.ToolTip', {
                    target: me.getEl(),
                    html: me.tooltip
            });
       	}
	},
	onDestroy: function() {
        var me = this;
		if(me.tooltip && me.tooltip.destroy) {
			me.tooltip.destroy();
		}
		me.callParent(arguments);
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
        var me = this;
		if(me.tooltip && me.tooltip.destroy) {
			me.tooltip.destroy();
		}
		if(text){
            me.tooltip = Ext.create('Ext.tip.ToolTip', {
                    target: me.getEl(),
                    html: text
            });
		}
	}
});
