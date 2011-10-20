Ext.define('itasks.component.show.Note',{
	extend: 'Ext.Component',
	alias: 'widget.itasks_show_note',
	initComponent: function() {
		if(!this.width && !this.hflex) {
			this.hflex = 1;
			this.minWidth = 500;
		}
		this.html = Ext.String.htmlEncode(this.value).replace("\n","<br>","g");
		
	}
});
