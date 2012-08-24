Ext.define('itwc.container.Viewport',{
	extend: 'Ext.container.Viewport',
	//Default top-level layout
	layout: 'itwc_box',
	halign: 'center',
	valign: 'middle',
	direction: 'vertical',
	padding: 0,
	autoScroll: true,
	initComponent: function() {
		this.layout = {type: 'itwc_box',halign: this.halign, valign: this.valign,direction: this.direction, padding: this.padding};
		this.callParent(arguments);
	}
});
