Ext.define('itwc.component.edit.Slider',{
	alias: 'widget.itwc_edit_slider',
	extend: 'Ext.slider.Single',
	mixins: ['itwc.Sizeable','itwc.component.edit.Editable'],

	itwcWidth: 'flex',	
	itwcMinWidth: 200,

	initComponent: function() {
        this.initSize();
		this.callParent(arguments);
		this.initEditable();
	}
});
