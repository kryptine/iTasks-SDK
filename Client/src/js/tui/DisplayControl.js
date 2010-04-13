Ext.ns('itasks.tui');

itasks.tui.DisplayControl = Ext.extend(Ext.Panel,{
	width: '100%',
	fieldClass: 'x-form-field',
	autoScroll: true,

	initComponent: function(){
		this.hideLabel = this.fieldLabel == null;
		this.fieldLabel = itasks.util.fieldLabel(this.optional,this.fieldLabel);
		this.unstyled = true;
		this.layout = 'form';

		this.panel = {
			xtype: 'panel',
			html: this.html,
			unstyled: true
		}
		
		delete this.html;
		
		Ext.apply(this,
		{ items: [this.panel]
		});	
		
		for(var i=0; i< this.formItems.length; i++){
			this.formItems[i][1][0].hidden = true;
			this.items[this.items.length] = this.formItems[i][1][0];
		}
		
		itasks.tui.DisplayControl.superclass.initComponent.apply(this,arguments);
	},
	
	afterRender: function(){
		itasks.tui.DisplayControl.superclass.afterRender.call(this,arguments);
		(this.moveActiveItems).defer(50,this);
	},
		
	moveActiveItems: function(){	
		for(var i=0; i< this.formItems.length; i++){
			var fitem = this.formItems[i];
			
			var phid 	= fitem[0];
			var elid  	= fitem[1][0].id;
			
			var ph 		= Ext.get(phid);
			var cmp 	= Ext.getCmp(elid);
			var el		= cmp.getEl();
			
			el.replace(ph);
			cmp.show();
			this.doLayout();
		}
		
		this.doLayout();
		this.findParentByType(itasks.ttc.FormContainer).doLayout();
	}
});

Ext.reg('itasks.tui.Display',itasks.tui.DisplayControl);

/*
placeDelayedItems : function(){
		for(var i=0; i < this.replaceItems.length; i++){
			var phid = this.replaceItems[i].applyToPHid;
			delete this.replaceItems[i].applyToPHid;
			
			var el = Ext.get(this.replaceItems[i].id);
			var phEl = Ext.get(phid);
			
			el.replace(phEl);
			el.removeClass('x-hidden');
		}
	},
	
	splitItems: function(data,ct){
		this.replaceItems = [];
	
		if(data instanceof Array){
			for(var i=0; i<data.length;i++){
				if(data[i].applyToPHid != null){
					data[i].cls = (data[i].cls || '')+' x-hidden';
					ct.replaceItems[ct.replaceItems.length] = data[i];
				}else	if(data[i].items){
					ct.splitItems(data[i].items,ct);
				}
			}
		}else {
			if(data.applyToPHid != null){
				data.cls = (data.cls || '')+' x-hidden';
				ct.replaceItems[ct.replaceItems.length] = data;
			}
			
			if(data.items != null){
				ct.splitItems(data.items,ct);
			}
		}
	}
*/