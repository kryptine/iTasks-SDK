Ext.ns("itasks.tui");

itasks.tui.FormButtonControl = itasks.tui.extendControl(Ext.Button,{
	height: 25,
	enableToggle: true,
	allowDepress: true,
	initComponent: function() {
		if(!this.width)
			this.hwrap = true;
			
		this.pressed = this.value == "Pressed";
	
		this.text = this.label;
		this.iconCls = this.icon;
		
		this.on('toggle',function(button,pressed){
			this.fireEvent('tuichange',this.taskId,this.name,pressed ? "Pressed" : "NotPressed");
		});
	
		itasks.tui.control.initComponent.apply(this,arguments);
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