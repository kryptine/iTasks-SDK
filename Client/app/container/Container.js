Ext.define('itwc.container.Container',{
	extend: 'Ext.container.Container',
	alias: 'widget.itwc_container',
    mixins: ['itwc.Sizeable'],
	requires: ['itwc.layout.container.Box'],

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

		this.layout = {type:'itwc_box', direction: this.direction, halign: this.halign, valign: this.valign, padding: this.padding};
		this.callParent(arguments);
	}
});
