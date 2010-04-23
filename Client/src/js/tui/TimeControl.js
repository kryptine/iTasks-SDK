Ext.ns("itasks.tui");

itasks.tui.TimeControl = Ext.extend(Ext.form.TimeField,{
	format: "H:i:s",
	width: 100,
	initComponent: function() {
		if(this.staticDisplay){
			this.autoCreate = {tag: 'div', style: 'padding-top: 4px', html: this.value};
		}
		
		this.hideLabel = this.fieldLabel == null;
		this.fieldLabel = itasks.util.fieldLabel(this.optional,this.fieldLabel);
		this.allowBlank = this.optional;
		if(this.value == "") delete this.value;
		itasks.tui.TimeControl.superclass.initComponent.apply(this,arguments);
	},
	
	afterRender: function(ct,position){		
		itasks.tui.TimeControl.superclass.afterRender.call(this,ct,position);

		if(this.staticDisplay){
			this.el.next().remove();		
		}	
	},
	
	setValue: function(value){
		if(this.staticDisplay){
			this.update(value);
		}else{
			itasks.tui.TimeControl.superclass.setValue.call(this,value);
		}
	}
});

Ext.reg("itasks.tui.Time",itasks.tui.TimeControl);