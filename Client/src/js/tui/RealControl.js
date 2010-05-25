Ext.ns("itasks.tui");

itasks.tui.RealControl = Ext.extend(Ext.form.NumberField,{
	width: 100,
	allowDecimals: true,
	decimalPrecision: 20, //Maximum precision
	initComponent: function() {
		if(this.staticDisplay){
			this.autoCreate = {tag: 'div', style: 'overflow: auto', html: this.value};
		}
		
		this.msgTarget = 'side';
		
		this.hideLabel = this.fieldLabel == null;
		this.fieldLabel = itasks.util.fieldLabel(this.optional,this.fieldLabel);
		this.allowBlank = this.optional;
		if(this.value == "") delete this.value;
		itasks.tui.RealControl.superclass.initComponent.apply(this,arguments);
	},
	
	afterRender: function(){
		itasks.tui.RealControl.superclass.afterRender.call(this,arguments);
		
		(function(){
			this.setError(this.errorMsg);
			this.setHint(this.hintMsg);
		}).defer(50,this);
	},
	
	setValue: function(value){
		if(this.staticDisplay){
			this.update(value);
		}else{
			itasks.tui.RealControl.superclass.setValue.call(this,value);
		}
		
		if(this.activeError) this.setError(this.activeError);
	},
	
	setError: function(msg){		
		(function() {
			if(msg == "") this.clearInvalid();
			else this.markInvalid(msg);
		}).defer(50,this);
	},
	
	setHint: function(msg){
		(function() {
			if(msg == "") itasks.tui.common.clearHint(this);
			else itasks.tui.common.markHint(this,msg);
		}).defer(50,this);
	}
});

Ext.reg("itasks.tui.Real",itasks.tui.RealControl);