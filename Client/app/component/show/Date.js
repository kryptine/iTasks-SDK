Ext.define('itasks.component.show.Date',{
	extend: 'Ext.Component',
	alias: 'widget.itasks_show_date',
	minHeight: 20,
	initComponent: function() {
		this.html = this.value;
	}
});
