Ext.ns("itasks.tui");

itasks.tui.TimeControl = itasks.tui.extendControl(Ext.form.TimeField,{
	format: "H:i:s",
	defaultWidth: 100,
	onChange: function () {
		if(this.isValid()) {
			var v = this.getRawValue();
			this.fireEvent('tuichange',this.taskId,this.name,v === "" ? null : v);
		}
	}
});

Ext.reg("itasks.tui.Time",itasks.tui.TimeControl);