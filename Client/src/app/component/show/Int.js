Ext.define('itasks.component.show.Int',{
	extend: 'Ext.Component',
	alias: 'widget.itasks.show.int',
	initComponent: function() {
		this.html = '' + this.value;
	}
});
