Ext.define('itasks.component.show.String',{
	extend: 'Ext.Component',
	alias: 'widget.itasks_show_string',
	style: 'white-space: nowrap;',
	minHeight: 20,
	initComponent: function() {
		this.html = Ext.util.Format.nl2br(Ext.util.Format.htmlEncode(this.value));
	}	
});
