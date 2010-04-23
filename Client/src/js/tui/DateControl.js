Ext.ns("itasks.tui");

itasks.tui.DateControl = Ext.extend(Ext.form.DateField,{
	format: "d-m-Y",
	initComponent: function() {
		if(this.staticDisplay){
			this.autoCreate = {tag: 'div', style: 'padding-top: 4px', html: this.value};
		}
			
		this.hideLabel = this.fieldLabel == null;
		this.fieldLabel = itasks.util.fieldLabel(this.optional,this.fieldLabel);
		this.allowBlank = this.optional;
		
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
	},
	setValue: function(value){
		if(this.staticDisplay){
			this.update(value);
		}else{
			itasks.tui.DateControl.superclass.setValue.call(this,value);
		}
	}
});

Ext.reg("itasks.tui.Date",itasks.tui.DateControl);