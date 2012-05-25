Ext.define('itasks.component.show.Int',{
	extend: 'Ext.Component',
	alias: 'widget.itasks_show_int',
	minHeight: 20,
	initComponent: function() {
		this.html = '' + this.value;
	},
	setValue: function(value){
		this.update('' + value);
	}	
});
