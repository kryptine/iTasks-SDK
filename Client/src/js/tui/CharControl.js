Ext.ns("itasks.tui");

itasks.tui.CharControl = Ext.extend(Ext.form.TextField,{
	width: 40,
	maxLength: 1,
	initComponent: function() {
		this.hideLabel = this.fieldLabel == null;
		this.fieldLabel = itasks.util.fieldLabel(this.optional,this.fieldLabel);
		this.allowBlank = this.optional;
		if(this.value == "") delete this.value;
		itasks.tui.StringControl.superclass.initComponent.apply(this,arguments);
	}
});

Ext.reg("itasks.tui.Char",itasks.tui.CharControl);