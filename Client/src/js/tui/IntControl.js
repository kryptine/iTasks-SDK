Ext.ns("itasks.tui");

itasks.tui.IntControl = Ext.extend(Ext.form.NumberField,{
	width: 100,
	allowDecimals: false,
	initComponent: function() {
		if(this.staticDisplay){
			this.autoCreate = {tag: 'div', style: 'overflow: auto', html: this.value};
		}
		
		this.hideLabel = this.fieldLabel == null;
		this.fieldLabel = itasks.util.fieldLabel(this.optional,this.fieldLabel);
		this.allowBlank = this.optional;
		if(this.value == "") delete this.value;
		itasks.tui.IntControl.superclass.initComponent.apply(this,arguments);
	},
	
	setValue: function(value){
		if(this.staticDisplay){
			this.update(value);
		}else{
			itasks.tui.IntControl.superclass.setValue.call(this,value);
		}
	}
	
	
});

Ext.reg("itasks.tui.Int",itasks.tui.IntControl);