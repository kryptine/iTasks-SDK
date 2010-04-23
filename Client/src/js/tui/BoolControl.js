Ext.ns("itasks.tui");

itasks.tui.BoolControl = Ext.extend(Ext.form.Checkbox,{
	initComponent: function() {
		if(this.staticDisplay){
			this.autoCreate = {tag: 'span', html: this.value};
		}
		
		this.hideLabel = this.fieldLabel == null;
		this.fieldLabel = itasks.util.fieldLabel(this.optional,this.fieldLabel);
		
		this.allowBlank = this.optional;
		this.checked = this.value == "True";
		
		if(this.value == "") delete this.value;
	
		itasks.tui.BoolControl.superclass.initComponent.apply(this,arguments);
	},
	
	setValue: function(value){
		if(this.staticDisplay){
			this.update(value);
		}else{		
			if(value == "True") value = true;
			itasks.tui.BoolControl.superclass.setValue.call(this,value);
		}
	}
});

Ext.reg("itasks.tui.Bool",itasks.tui.BoolControl);