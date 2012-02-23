Ext.define('itasks.component.Time',{
	extend: 'Ext.form.field.Time',
	mixins: ['itasks.mixin.Editable'],
	alias: 'widget.itime',
	format: 'H:i:s',
	editBufferTime: 1000,
	getEditValue: function() {
		return this.getRawValue();
	}
});