Ext.ns("itasks.tui");

itasks.tui.TimeControl = itasks.tui.extendControl(Ext.form.TimeField,{
	defaultWidth: ['Fixed',100],
	defaultHeight: ['Fixed',25],
	format: "H:i:s",
	onChange: function () {
		if(this.isValid()) {
			var v = this.getRawValue();
			this.fireEvent('tuichange',this.taskId,this.name,v === "" ? null : v);
		}
	}
});

Ext.reg("itasks.tui.Time",itasks.tui.TimeControl);