Ext.define('itwc.component.view.HTML',{
	extend: 'Ext.Component',
	alias: 'widget.itwc_view_html',
	initComponent: function() {
	
		this.html = this.value;	
		this.callParent(arguments);
	},
	setValue: function(value) {
		this.getEl().update(value);
	}
});
