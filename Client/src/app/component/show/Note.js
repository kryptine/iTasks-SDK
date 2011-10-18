Ext.define('itasks.component.show.Note',{
	extend: 'Ext.Component',
	alias: 'widget.itasks.show.note',
	minWidth: 500,
	minHeight: 100,
	initComponent: function() {
		this.html = this.value;
	}
});
