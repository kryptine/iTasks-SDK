Ext.ns("itasks.tui");

itasks.tui.TimeControl = Ext.extend(Ext.form.TimeField,{
	format: "H:i:s",
	width: 100,
	initComponent: function() {
		this.msgTarget = 'side';
		this.listeners = {change: {fn: this.onChange, scope: this}};
		
		this.hideLabel = this.fieldLabel == null;
		this.fieldLabel = itasks.util.fieldLabel(this.optional,this.fieldLabel);
		
		
		if(this.value == "") delete this.value;
		itasks.tui.TimeControl.superclass.initComponent.apply(this,arguments);
		
		this.addEvents('tuichange');
		this.enableBubble('tuichange');
	},
	onChange: function() {
		this.fireEvent('tuichange',this.taskId,this.name,this.getRawValue());
	},
	afterRender: function(ct,position){		
		itasks.tui.TimeControl.superclass.afterRender.call(this,ct,position);

		if(this.errorMsg)
			itasks.tui.common.markError(this,this.errorMsg);
		else if(this.hintMsg)
			itasks.tui.common.markHint(this,this.hintMsg);
	},
	setValue: function(value){
		itasks.tui.TimeControl.superclass.setValue.call(this,value);
		if(this.activeError)
			this.setError(this.activeError);
	},
	setError: function(msg){
		if(msg == "")
			itasks.tui.common.clearError(this);
		else
			itasks.tui.common.markError(this,msg);
	},
	setHint: function(msg){
		if(msg == "")
			itasks.tui.common.clearHint(this);
		else
			itasks.tui.common.markHint(this,msg);
	}
});

Ext.reg("itasks.tui.Time",itasks.tui.TimeControl);