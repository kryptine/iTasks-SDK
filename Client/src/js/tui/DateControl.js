Ext.ns("itasks.tui");

itasks.tui.DateControl = itasks.tui.extendControl(Ext.form.DateField,{
	defaultWidth: ['Fixed',100],
	defaultHeight: ['Fixed',25],
	format: "Y-m-d",
	onChange: function () {
		if(this.isValid()) {
			var v = this.getRawValue();
			this.fireEvent('tuichange',this.taskId,this.name,v === "" ? null : v);
		}
	}
});

Ext.reg("itasks.tui.Date",itasks.tui.DateControl);