Ext.define('itwc.component.edit.Int',{
	alias: 'widget.itwc_edit_int',
	extend: 'Ext.form.field.Number',
	mixins: ['itwc.component.edit.Editable'],
	allowDecimals: false,
	hideTrigger: true,
    validateOnChange: false,
    validateOnBlur: false,
	initComponent: function() {
		this.callParent(arguments);
		this.initEditable();
	}
});
