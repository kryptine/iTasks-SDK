Ext.ns("itasks.tui");

itasks.tui.RealControl = Ext.extend(Ext.form.NumberField,{
	width: 100,
	allowDecimals: true,
	decimalPrecision: 20, //Maximum precision
	initComponent: function() {
		this.msgTarget = 'side';
		this.listeners = {change: {fn: this.onChange, scope: this}};
		
		this.hideLabel = this.fieldLabel == null;
		this.fieldLabel = itasks.util.fieldLabel(this.optional,this.fieldLabel);
		
		if(this.value == "") delete this.value;
		itasks.tui.RealControl.superclass.initComponent.apply(this,arguments);
		
		this.addEvents('tuichange');
		this.enableBubble('tuichange');
	},
	onChange: function() {
		this.fireEvent('tuichange',this.name,this.getValue().toString());
	},
	afterRender: function(){
		itasks.tui.RealControl.superclass.afterRender.call(this,arguments);
		
		if(this.errorMsg)
			itasks.tui.common.markError(this,this.errorMsg);
		else if(this.hintMsg)
			itasks.tui.common.markHint(this,this.hintMsg);
	},
	setValue: function(value){
		itasks.tui.RealControl.superclass.setValue.call(this,value);
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

Ext.reg("itasks.tui.Real",itasks.tui.RealControl);