Ext.ns("itasks.tui");

itasks.tui.DateControl = Ext.extend(Ext.form.DateField,{
	format: "d-m-Y",
	initComponent: function() {
		if(this.staticDisplay){
			this.autoCreate = {tag: 'div', style: 'padding-top: 4px', html: this.value};
		}
		
		this.msgTarget = 'side';
			
		this.hideLabel = this.fieldLabel == null;
		this.fieldLabel = itasks.util.fieldLabel(this.optional,this.fieldLabel);
		//this.allowBlank = this.optional;
		
		itasks.tui.DateControl.superclass.initComponent.apply(this,arguments);		
	},
	//getValue: function() {
		//return this.getRawValue();
	//},
	afterRender: function(ct,position){		
		itasks.tui.DateControl.superclass.afterRender.call(this,ct,position);

		if(this.staticDisplay){
			this.el.next().remove();		
		}	
		
		(function(){
			this.setError(this.errorMsg);
			this.setHint(this.hintMsg);
		}).defer(50,this);
	},
	setValue: function(value){
		if(this.staticDisplay){
			this.update(value);
		}else{
			itasks.tui.DateControl.superclass.setValue.call(this,value);
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

Ext.reg("itasks.tui.Date",itasks.tui.DateControl);