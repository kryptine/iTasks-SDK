Ext.ns("itasks.tui");

itasks.tui.DateControl = Ext.extend(Ext.form.DateField,{
	format: "d-m-Y",
	initComponent: function() {
		this.hideLabel = this.fieldLabel == null;
		this.fieldLabel = itasks.util.fieldLabel(this.optional,this.fieldLabel);
		this.allowBlank = this.optional;
		if(this.value == "") delete this.value;
		itasks.tui.DateControl.superclass.initComponent.apply(this,arguments);
	},
	getValue: function() {
		return this.getRawValue();
	}
});

Ext.reg("itasks.tui.Date",itasks.tui.DateControl);