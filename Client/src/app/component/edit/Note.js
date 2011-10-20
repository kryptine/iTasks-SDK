Ext.define('itasks.component.edit.Note',{
	alias: 'widget.itasks_edit_note',
	extend: 'Ext.form.field.TextArea',
	mixins: ['itasks.mixin.Editable'],
	initComponent: function() {
		if(!this.width && !this.hflex) {
			this.hflex = 1;
			this.minWidth = 500;
		}
		this.callParent(arguments);
	}
});
