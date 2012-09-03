Ext.define('itwc.component.view.String',{
	extend: 'Ext.Component',
	alias: 'widget.itwc_view_string',
	initComponent: function() {
		//Replace newlines by <br>'s and html encode each line
		if(this.value) {
			this.html = (Ext.Array.map(this.value.split("\n"),Ext.htmlEncode)).join("<br>");
		} else {
			this.html = "";
		}
		this.callParent(arguments);
	}
});
