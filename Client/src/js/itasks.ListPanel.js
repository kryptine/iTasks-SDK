Ext.ns("itasks");

itasks.ListPanel = Ext.extend(Ext.Panel,
{
	
	selectedItems : {},
	
	initComponent: function(){
				
		Ext.apply(this,
		{ autoHeight: true
		, border: false
		, cls: 'listPanel'
		, bodyCfg: {
			id: 'sortable_'+this.id,
			tag: 'ul'		
		}
		, buttons: [
			{ text: 'Add'
			, name: this.name
			, value: 'add'
			},
			{ text: 'Remove'
			, name: this.name
			}
		]	
		});	

		var workCt = this.findParentByType('itasks.work');
		
		itasks.ListPanel.superclass.initComponent.apply(this,arguments);
	},
	
	afterRender: function(arguments){
		itasks.ListPanel.superclass.afterRender.call(this,arguments);
		this.doLayout();
		this.delayedTask = new Ext.util.DelayedTask();	
		
		if(this.editable){
			Sortable.create('sortable_'+this.id,{onUpdate: this.onUpdateCB});
		}
	
		this.buttons[1].clickCB = this.remItemHandler;
	},
	
	onUpdateCB: function(ct){
		
		var id = ct.id.substring(ct.id.lastIndexOf('_')+1);		
		var lp = Ext.getCmp(id);
		
		var task = function(){
			lp.disable();
			var formCt = lp.findParentByType("itasks.task-ext-form");
			formCt.addUpdate(lp.name,"ord_"+Sortable.sequence(ct.id));
			formCt.sendUpdates(false);		
		}
		
		lp.delayedTask.delay(1000,task,lp);
	},
	
	deselectAll: function(){
		var f = function(){
			this.blur();
			if(this.deselect){
				this.deselect();			
			}
		}
		
		this.selectedItems = {};
		this.cascade(f);
	},

	selectItem: function(index){
		this.selectedItems[index] = true;
	},
	
	deselectItem: function(index){
		this.selectedItems[index] = false;
	},

	remItemHandler: function(b){
		var ct = this.findParentByType('itasks.listpanel');
		var val = 'rem_'
	
		for(x in ct.selectedItems){
			if(ct.selectedItems[x]){
				val += x+','
			}
		}
		
		b.value = val.substring(0,val.length-1);
	}
});

itasks.ListItem = Ext.extend(Ext.Panel,
{	
	initComponent: function(arguments){
		
		Ext.apply(this,
		{ border: false
		, cls: 'listPanelItem'
		, autoHeight: true
		, autoEl : 'li'
		, selected: 'False'
		});
		
		itasks.ListItem.superclass.initComponent.apply(this,arguments);
		
		var index = this.id.lastIndexOf('_');
		this.index = this.id.substring(index+1);
	},
	
	afterRender: function(arguments){
		itasks.ListItem.superclass.afterRender.call(this,arguments);
		this.body.on("mousedown", this.clickHandler ,this);
	},
	
	clickHandler : function(node,e){
		var p = this.findParentByType('itasks.listpanel');
	
		if(this.selected){
			this.deselect();
			p.deselectItem(this.index);
		}else{
			p.deselectAll();
			p.selectItem(this.index);
			this.select();
		}	
	},
	
	select: function(){
		this.selected = true;
		this.addClass('listPanelItemSelected');
	},
	
	deselect: function(){
		this.selected = false;
		this.removeClass('listPanelItemSelected');
	}
});

Ext.reg("itasks.listitem",itasks.ListItem);
Ext.reg("itasks.listpanel",itasks.ListPanel);