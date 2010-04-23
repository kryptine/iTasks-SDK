Ext.ns("itasks.tui");

itasks.tui.StringControl = Ext.extend(Ext.form.TextField,{
	width: 330,
	initComponent: function() {
		if(this.staticDisplay){
			this.autoCreate = {tag: 'div', style: 'overflow: auto', html: this.value};
		}
		
		this.hideLabel = this.fieldLabel == null;
		this.fieldLabel = itasks.util.fieldLabel(this.optional,this.fieldLabel);
		this.allowBlank = this.optional;
		if(this.value == "") delete this.value;
		itasks.tui.StringControl.superclass.initComponent.apply(this,arguments);
	},
	setValue: function(value){		
		if(this.staticDisplay){
			this.update(value);
		}else{
			itasks.tui.StringControl.superclass.setValue.call(this,value);
		}
	}
});

Ext.reg("itasks.tui.String",itasks.tui.StringControl);