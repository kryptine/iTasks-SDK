Ext.ns("itasks.tui");

itasks.tui.TimeControl = Ext.extend(Ext.form.TimeField,{
	format: "H:i:s",
	initComponent: function() {
		this.hideLabel = this.fieldLabel == null;
		this.fieldLabel = itasks.util.fieldLabel(this.optional,this.fieldLabel);
		this.allowBlank = this.optional;
		if(this.value == "") delete this.value;
		itasks.tui.TimeControl.superclass.initComponent.apply(this,arguments);
	}
});

Ext.reg("itasks.tui.Time",itasks.tui.TimeControl);