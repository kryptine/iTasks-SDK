Ext.define('itasks.component.show.Note',{
	extend: 'Ext.Component',
	alias: 'widget.itasks_show_note',
	minWidth: 500,
	minHeight: 100,
	initComponent: function() {
		this.html = this.value;
	}
});
