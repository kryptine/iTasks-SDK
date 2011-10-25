Ext.define('itasks.component.show.Note',{
	extend: 'Ext.Component',
	alias: 'widget.itasks_show_note',
	style: 'width: 400px',
	initComponent: function() {
		if(!this.width && !this.hflex) {
			this.hflex = 1;
			this.minWidth = 400;
		}
		if(this.value) {
			this.html = Ext.String.htmlEncode(this.value).replace("\n","<br>","g");
		} else {
			this.html = "";
		}
	}
});
