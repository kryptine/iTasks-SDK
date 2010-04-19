Ext.ns("itasks.tui");

itasks.tui.FormattedTextControl = Ext.extend(Ext.form.HtmlEditor,{
	width: 700,
	height: 300,
	initComponent: function() {
		this.hideLabel = this.fieldLabel == null;
		this.fieldLabel = itasks.util.fieldLabel(this.optional,this.fieldLabel);
		this.allowBlank = this.optional;
		itasks.tui.FormattedTextControl.superclass.initComponent.apply(this,arguments);
	},
	getValue: function() {
		return this.getRawValue();
	}
});

Ext.reg("itasks.tui.FormattedText",itasks.tui.FormattedTextControl);