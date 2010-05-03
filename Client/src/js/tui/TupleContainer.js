Ext.ns('itasks.tui');

itasks.tui.TupleContainer = Ext.extend(Ext.Container,{

	initComponent: function(){
		
		this.items = [];
		this.unstyled = true;
		this.hideLabel = this.fieldLabel == null;
		this.fieldLabel = itasks.util.fieldLabel(this.optional,this.fieldLabel);
		this.autoHeight = true;
		//this.autoEl = {tag: 'ul' }
		
		for(var i=0; i<this.definitions.length; i++){
			/*this.items[i] = {
				//xtype: 'container',
				//autoEl: {tag: 'li', style: 'padding: 0px 4px 0px 0px; width: 100%'},
				layout: 'fit',
				items: this.definitions[i],
				unstyled: true,
				autoHeight: true
			}*/
			
			this.items[i] = this.definitions[i];
		}
			
		itasks.tui.TupleContainer.superclass.initComponent.apply(this,arguments);
	},

	afterRender: function(){
		itasks.tui.TupleContainer.superclass.afterRender.call(this,arguments);
		//this.doLayoutComponents.defer(50,this);
	},

	doLayoutComponents: function(){
		var w = this.getWidth();
		var iw = 0;
		
		for(var i=0; i<this.items.length; i++){
			iw += this.getComponent(i).getWidth();
		}

		//if(iw > w){
			for(var i=0; i<this.items.length; i++){
				this.getComponent(i).setWidth(w);
			}
		//}
	}

});

Ext.reg('itasks.tui.Tuple',itasks.tui.TupleContainer);