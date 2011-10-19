Ext.define('itasks.component.show.Int',{
	extend: 'Ext.Component',
	alias: 'widget.itasks_show_int',
	initComponent: function() {
		this.html = '' + this.value;
	}
});
