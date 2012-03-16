Ext.define('itasks.component.show.Real',{
	extend: 'Ext.Component',
	alias: 'widget.itasks_show_real',
	minHeight: 20,
	initComponent: function() {
		this.html = '' + this.value;
	}
});
