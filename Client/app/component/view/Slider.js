Ext.define('itwc.component.view.Slider',{
	alias: 'widget.itwc_view_slider',
	extend: 'Ext.slider.Single',
    mixins: ['itwc.Sizeable'],
	disabled: true,

	itwcWidth: 'flex',	
	itwcMinWidth: 200,

    initComponent: function() {
        this.initSize();
        this.callParent();
    }
});
