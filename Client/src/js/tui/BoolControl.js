Ext.ns("itasks.tui");

itasks.tui.BoolControl = Ext.extend(Ext.form.Checkbox,{
	initComponent: function() {
		this.hideLabel = this.fieldLabel == null;
		this.fieldLabel = itasks.util.fieldLabel(this.optional,this.fieldLabel);
		this.allowBlank = this.optional;
		this.checked = this.value == "True";
		if(this.value == "") delete this.value;
		itasks.tui.BoolControl.superclass.initComponent.apply(this,arguments);
	}
});

Ext.reg("itasks.tui.Bool",itasks.tui.BoolControl);