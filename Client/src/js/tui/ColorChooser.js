Ext.ns("itasks.tui");

itasks.tui.ColorChooser = Ext.extend(Ext.ColorPalette, {
	initComponent: function() {
		Ext.apply(this, {
			hideLabel: this.fieldLabel == null,
			fieldLabel: itasks.util.fieldLabel(this.optional, this.fieldLabel),
			allowBlank: this.optional,
			style: {width: '150px'}
		});
		itasks.tui.ColorChooser.superclass.initComponent.apply(this,arguments);
	},
	getValue: function() {
		return this.value;
	}
});

Ext.reg("itasks.tui.ColorChooser", itasks.tui.ColorChooser);