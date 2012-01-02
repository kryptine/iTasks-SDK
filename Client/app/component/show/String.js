Ext.define('itasks.component.show.String',{
	extend: 'Ext.Component',
	alias: 'widget.itasks_show_string',
	style: 'white-space: nowrap;',
	initComponent: function() {
		this.html = Ext.String.htmlEncode(this.value);
	}
});
