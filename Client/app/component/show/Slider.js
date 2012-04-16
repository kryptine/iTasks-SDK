Ext.define('itasks.component.show.Slider',{
	extend: 'Ext.slider.Single',
	alias: 'widget.itasks_show_slider',
	mixins: ['itasks.mixin.Editable'],
	initComponent: function() {
		this.width = 200;
		this.height = 20;
		this.minHeight = 20;
		this.disabled = true;
		this.callParent(arguments);
	}
});
