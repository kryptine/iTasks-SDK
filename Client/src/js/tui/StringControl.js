Ext.ns("itasks.tui");

itasks.tui.StringControl = Ext.extend(Ext.form.TextField,{
	width: 330,
	initComponent: function() {
		if(this.staticDisplay){
			this.autoCreate = {tag: 'div', style: 'overflow: auto', html: this.value};
		}
		
		this.msgTarget = 'side';
		
		this.hideLabel = this.fieldLabel == null;	
		this.fieldLabel = itasks.util.fieldLabel(this.optional,this.fieldLabel);
		//this.allowBlank = this.optional;
		this.allowBlank = true;
		if(this.value == "") delete this.value;
		itasks.tui.StringControl.superclass.initComponent.apply(this,arguments);
	},
	
	afterRender: function(){
		itasks.tui.StringControl.superclass.afterRender.call(this,arguments);
		
		(function(){
			this.setError(this.errorMsg);
			this.setHint(this.hintMsg);
		}).defer(50,this);
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
		(function() {
			if(msg == "") itasks.tui.common.clearError(this);
			else itasks.tui.common.markError(this,msg);
		}).defer(50,this);
	},
	
	setHint: function(msg){
		(function() {
			if(msg == "") itasks.tui.common.clearHint(this);
			else itasks.tui.common.markHint(this,msg);
		}).defer(50,this);
	}
});

Ext.reg("itasks.tui.String",itasks.tui.StringControl);