Ext.define('itasks.component.edit.Slider',{
	extend: 'Ext.slider.Single',
	alias: 'widget.itasks_edit_slider',
	mixins: ['itasks.mixin.Editable'],
	initComponent: function() {
		this.width = 200;
		this.height = 20;
		this.minHeight = 20;
		this.callParent(arguments);
	}
});
