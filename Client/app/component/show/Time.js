Ext.define('itasks.component.show.Time',{
	extend: 'Ext.Component',
	alias: 'widget.itasks_show_time',
	minHeight: 20,
	initComponent: function() {
		this.html = this.value;
	}
});
