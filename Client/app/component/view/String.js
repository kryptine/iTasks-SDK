Ext.define('itwc.component.view.String',{
	extend: 'Ext.Component',
	alias: 'widget.itwc_view_string',
	initComponent: function() {
		//Replace newlines by <br>'s and html encode each line
		if(this.value) {
			this.html = this.toHtml(this.value);
		} else {
			this.html = "";
		}
		this.callParent(arguments);
	},
	toHtml: function(value) {
		return (Ext.Array.map(value.split("\n"),Ext.htmlEncode)).join("<br>");
	},
	setValue: function(value) {
		if(this.rendered) {
			this.el.update(this.toHtml(value));
			if(this.ownerCt) {
				this.ownerCt.doLayout();
			}
		} else {
			this.value = value;
		}
	}
});
