Ext.ns("itasks.tui");

itasks.tui.BoolControl = itasks.tui.extendControl(Ext.form.Checkbox,{
	defaultWidth: ['Fixed',15],
	defaultHeight: ['Fixed',15],
	initComponent: function() {
		this.checked = this.value;
		this.listeners = {check: this.onChange, scope: this};
		itasks.tui.control.initComponent.apply(this,arguments);
	}
});

Ext.reg("itasks.tui.Bool",itasks.tui.BoolControl);