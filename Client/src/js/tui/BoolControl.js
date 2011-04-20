Ext.ns("itasks.tui");

itasks.tui.BoolControl = itasks.tui.extendBase(Ext.form.Checkbox,{
	initComponent: function() {
		this.checked = this.value;
		this.listeners = {check: this.onChange, scope: this};
		itasks.tui.base.initComponent.apply(this,arguments);
	}
});

Ext.reg("itasks.tui.Bool",itasks.tui.BoolControl);