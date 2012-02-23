Ext.define('itasks.component.HtmlDisplay',{
	extend: 'Ext.Component',
	alias: 'widget.ihtml',
	//style: 'white-space: nowrap',
	initComponent: function() {
		this.html = this.value;
		this.callParent(arguments);
	},
	afterRender: function() {
		this.callParent(arguments);
		
		if (this.tooltip) {
			new Ext.ToolTip({html: this.tooltip, target: this.getEl()});
		}
	},
	setValue: function(html) {
		console.log(html);
		var el = this.el || this.getEl();
		el.update(html);
	}
});