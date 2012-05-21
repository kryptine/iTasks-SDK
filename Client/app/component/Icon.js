Ext.define('itasks.component.Icon', {
	extend: 'Ext.Component',
	alias: 'widget.itasks_icon',
	width: 16,
	height: 16,
	initComponent: function() {
		this.addCls(this.type || 'icon-help');
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
	setType: function(type){
		this.removeCls(this.type);
		this.type = type;
		this.addCls(this.type || 'icon-help');
	}	
});
