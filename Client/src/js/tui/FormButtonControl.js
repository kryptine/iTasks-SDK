Ext.ns("itasks.tui");

itasks.tui.FormButtonControl = Ext.extend(Ext.Button,{
	initComponent: function() {
		if(this.staticDisplay){
			this.autoCreate = {tag: 'span', html: this.value};
		}
		
		this.hideLabel = this.fieldLabel == null;
		this.fieldLabel = itasks.util.fieldLabel(this.optional,this.fieldLabel);
		
		this.enableToggle = true;
		this.allowDepress = true;
		this.pressed = this.value == "True";
	
		this.text = this.label;
		this.iconCls = this.icon;
		
		this.on('toggle',function(button,pressed){
			this.value = (pressed)?"true":"";
		});
	
		itasks.tui.FormButtonControl.superclass.initComponent.apply(this,arguments);
	},
	
	setValue: function(value){
		if(this.staticDisplay){
			this.update(value);
		}else if(value == "True"){
			this.toggle(true,true);
		}else{
			this.toggle(false,true);
		}
	}
});

Ext.reg("itasks.tui.FormButton",itasks.tui.FormButtonControl);