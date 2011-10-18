Ext.define('itasks.component.edit.Time',{
	extend: 'Ext.form.field.Time',
	alias: 'widget.itasks.edit.time',
	mixins: ['itasks.mixin.Editable'],
	format: 'H:i:s',
	editBufferTime: 1000,
	getEditValue: function() {
		return this.getRawValue();
	}
});
