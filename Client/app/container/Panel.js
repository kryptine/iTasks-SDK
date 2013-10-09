Ext.define('itwc.container.Panel',{
	extend: 'Ext.panel.Panel',
	alias: 'widget.itwc_panel',
	requires: ['itwc.layout.container.Box'],
	mixins: ['itwc.Sizeable','itwc.container.HotkeyArea'],
	
	//Default container config
	layout: 'itwc_box',
	halign: 'left',
	valign: 'top',
	direction: 'vertical',
	padding: 0,

	//Default dimensions
	itwcWidth: 'flex',
	itwcHeight: 'flex',
	itwcMinWidth: 'wrap',
	itwcMinHeight: 'wrap',

	autoScroll: true,

	initComponent: function() {

        this.initSize();
		//Set shrinkWrap using width & height values
		this.shrinkWrap = (this.itwcWidth === 'wrap' ? 1 : 0) | (this.itwcHeight === 'wrap' ? 2 : 0);

		this.layout = {type:'itwc_box', direction: this.direction, halign: this.halign, valign: this.valign, padding: 0};
		this.callParent(arguments);
	},
	afterRender: function() {
		this.callParent(arguments);
		this.initHotkeys();
	},
	onDestroy: function () {
		this.destroyHotkeys();
		this.callParent(arguments);
	}
});
