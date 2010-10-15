Ext.ns("itasks.tui");

itasks.tui.StringControl = Ext.extend(Ext.form.TextField,{
	width: 330,
	initComponent: function() {
		if(this.staticDisplay){
			this.autoCreate = {tag: 'div', style: 'overflow: auto', html: this.value};
		}
		
		this.msgTarget = 'side';
		this.listeners = {change : {fn: this.onChange, scope: this}};
		
		this.hideLabel = this.fieldLabel == null;	
		this.fieldLabel = itasks.util.fieldLabel(this.optional,this.fieldLabel);
		
		this.allowBlank = true;
		if(this.value == "") delete this.value;
	
		itasks.tui.StringControl.superclass.initComponent.apply(this,arguments);
		
		this.addEvents('tuichange');
		this.enableBubble('tuichange');
	},
	onChange: function() {
		this.fireEvent('tuichange', this.name, this.getValue());
	},
	afterRender: function() {
		itasks.tui.StringControl.superclass.afterRender.call(this,arguments);
		
		if(this.errorMsg)
			itasks.tui.common.markError(this,this.errorMsg);
		else if(this.hintMsg) {
			itasks.tui.common.markHint(this,this.hintMsg)
		}
	},
	setValue: function(value){		
		if(this.staticDisplay){
			this.update(value);
		}else{
			itasks.tui.StringControl.superclass.setValue.call(this,value);
		}	
		if(this.activeError) this.setError(this.activeError);
	},
	
	setError: function(msg){		
		if(msg == "") {
			itasks.tui.common.clearError(this);
		} else {
			itasks.tui.common.markError(this,msg);
		}
	},
	
	setHint: function(msg){
		if(msg == "") {	
			itasks.tui.common.clearHint(this);
		} else {
			itasks.tui.common.markHint(this,msg);
		}
	}
});

Ext.reg("itasks.tui.String",itasks.tui.StringControl);