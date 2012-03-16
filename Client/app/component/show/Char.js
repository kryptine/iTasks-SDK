Ext.define('itasks.component.show.Char',{
	extend: 'Ext.Component',
	alias: 'widget.itasks_show_char',
	minHeight: 20,
	initComponent: function() {
		this.html = this.value;
	}
});
