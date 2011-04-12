Ext.ns("itasks.tui");

itasks.tui.FormButtonControl = itasks.tui.extendBase(Ext.Button,{
	enableToggle: true,
	allowDepress: true,
	initComponent: function() {
		this.pressed = this.value == "Pressed";
	
		this.text = this.label;
		this.iconCls = this.icon;
		
		this.on('toggle',function(button,pressed){
			this.fireEvent('tuichange',this.taskId,this.name,pressed ? "Pressed" : "NotPressed");
		});
	
		itasks.tui.base.initComponent.apply(this,arguments);
	},
	
	setValue: function(value){
		if(value == "Pressed"){
			this.toggle(true,true);
		}else{
			this.toggle(false,true);
		}
	}
});

Ext.reg("itasks.tui.FormButton",itasks.tui.FormButtonControl);