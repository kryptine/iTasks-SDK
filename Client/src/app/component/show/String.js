Ext.define('itasks.component.show.String',{
	extend: 'Ext.Component',
	alias: 'widget.itasks.show.string',
	style: 'white-space: nowrap;',
	initComponent: function() {
		this.html = Ext.String.htmlEncode(this.value);
	}
});
