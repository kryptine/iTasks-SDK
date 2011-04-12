Ext.ns("itasks.tui");

itasks.tui.DateControl = itasks.tui.extendBase(Ext.form.DateField,{
	format: "Y-m-d",
	onChange: function () {
		if(this.isValid()) {
			var v = this.getRawValue();
			this.fireEvent('tuichange',this.taskId,this.name,v === "" ? null : v);
		}
	}
});

Ext.reg("itasks.tui.Date",itasks.tui.DateControl);