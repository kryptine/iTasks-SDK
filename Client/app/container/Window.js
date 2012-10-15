Ext.define('itwc.container.Window',{
	extend: 'Ext.window.Window',
	alias: 'widget.itwc_window',
	requires: ['itwc.layout.container.Box'],
	autoScroll: true,
	
	//Default container config
	layout: 'itwc_box',
	halign: 'left',
	valign: 'top',
	direction: 'vertical',
	padding: 0,

	//Default dimensions
	width: 'flex',
	height: 'flex',
	minWidth: 'wrap',
	minHeight: 'wrap',
	
	//Reference back to the panel that created this window
	panel: null,

	initComponent: function() {
		
		//Set closable only if an action and task id are supplied
		this.closable = false;
	
		//Set shrinkWrap using width & height values		
		this.shrinkWrap = (this.width === 'wrap' ? 1 : 0) | (this.height === 'wrap' ? 2 : 0);
		
		this.layout = {type:'itwc_box', direction: this.direction, halign: this.halign, valign: this.valign, padding: this.padding};
		
		this.callParent(arguments);
	}
});
