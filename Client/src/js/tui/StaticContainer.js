Ext.ns('itasks.tui');

itasks.tui.StaticContainer = Ext.extend(Ext.Container,{

	initComponent: function(){
		
		this.unstyled = true;
		this.autoHeight = true;
		
		this.layout = 'form';
				
		if(this.fieldLabel == null) {
			delete this.fieldLabel;
		} else {
			this.fieldLabel = itasks.util.fieldLabel(this.optional,this.fieldLabel);
		}	
		itasks.tui.StaticContainer.superclass.initComponent.apply(this,arguments);
	},
	afterRender: function(){
		itasks.tui.StaticContainer.superclass.afterRender.call(this,arguments);
	},
	getPreferredWidth : function(){
		var w = 0;
		
		for(var i=0; i < this.items.length; i++){
			var itm = this.items.get(i);
			if(itm.getPreferredWidth) w += itm.getPreferredWith();
		}
		
		return (w>700)?700:w
	},
	setPreferredWidth : function(width){
		this.setWidth(width);
		
		for(var i=0; i < this.items.length; i++){
			var itm = this.items.get(i);
			if(itm.getWidth() > this.getWidth()) this.setAutoScroll(true);
		}
	}
});

Ext.reg('itasks.tui.Static',itasks.tui.StaticContainer);