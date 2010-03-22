Ext.ns("itasks.tui");

itasks.tui.RealControl = Ext.extend(Ext.form.NumberField,{
	width: 100,
	allowDecimals: true,
	decimalPrecision: 20, //Maximum precision
	initComponent: function() {
		this.hideLabel = this.fieldLabel == null;
		this.fieldLabel = itasks.util.fieldLabel(this.optional,this.fieldLabel);
		this.allowBlank = this.optional;
		if(this.value == "") delete this.value;
		itasks.tui.RealControl.superclass.initComponent.apply(this,arguments);
	}
});

Ext.reg("itasks.tui.Real",itasks.tui.RealControl);