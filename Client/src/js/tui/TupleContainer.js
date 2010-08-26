Ext.ns('itasks.tui');

itasks.tui.TupleContainer = Ext.extend(Ext.Container,{

	initComponent: function(){
		
		this.unstyled = true;
		this.autoHeight = true;
		
		this.layout = 'form'
				
		if(this.fieldLabel == null) delete this.fieldLabel;
		else this.fieldLabel = itasks.util.fieldLabel(this.optional,this.fieldLabel);
			
		itasks.tui.TupleContainer.superclass.initComponent.apply(this,arguments);
	},

	afterRender: function(){
		itasks.tui.TupleContainer.superclass.afterRender.call(this,arguments);
		
		/*for(var i=0; i < this.items.length; i++){
			var itm = this.items.get(i);
			itm.addClass('tuple-item');
		}*/
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

Ext.reg('itasks.tui.Tuple',itasks.tui.TupleContainer);