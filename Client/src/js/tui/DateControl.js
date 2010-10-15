Ext.ns("itasks.tui");

itasks.tui.DateControl = Ext.extend(Ext.form.DateField,{
	format: "Y-m-d",
	initComponent: function() {
		if(this.staticDisplay){
			this.autoCreate = {tag: 'div', style: 'padding-top: 4px', html: this.value};
		}
		
		this.msgTarget = 'side';
		this.listeners = {change: {fn: this.onChange, scope: this}};
			
		this.hideLabel = this.fieldLabel == null;
		this.fieldLabel = itasks.util.fieldLabel(this.optional,this.fieldLabel);

		itasks.tui.DateControl.superclass.initComponent.apply(this,arguments);		
		this.addEvents('tuichange');
		this.enableBubble('tuichange');
	},
	onChange: function () {
		this.fireEvent('tuichange',this.name, this.getRawValue());
	},
	afterRender: function(ct,position){		
		itasks.tui.DateControl.superclass.afterRender.call(this,ct,position);

		if(this.staticDisplay){
			this.el.next().remove();		
		}	
		if(this.errorMsg)
			itasks.tui.common.markError(this,this.errorMsg);
		else if(this.hintMsg)
			itasks.tui.common.markHint(this,this.hintMsg);
	},
	setValue: function(value){
		if(this.staticDisplay){
			this.update(value);
		}else{
			itasks.tui.DateControl.superclass.setValue.call(this,value);
		
			if(this.activeError)
				this.setError(this.activeError);
		}
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

Ext.reg("itasks.tui.Date",itasks.tui.DateControl);