Ext.define('itwc.component.edit.Date',{
	alias: 'widget.itwc_edit_date',
	extend: 'Ext.form.field.Date',
	mixins: ['itwc.Sizeable','itwc.component.edit.Editable'],

	itwcWidth: 100,

	format: 'Y-m-d',
	validateOnChange: false,
	initComponent: function() {
        this.initSize();
		this.callParent(arguments);
		this.initEditable();
	},
	getEditorValue: function() {
		return this.getRawValue();
	},
	validate: function() {
		return true;
	}
});
