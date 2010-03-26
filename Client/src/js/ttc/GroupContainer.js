Ext.ns('itasks.ttc')

itasks.ttc.GroupContainer = Ext.extend(Ext.Panel,{

	initComponent: function(){
		Ext.apply(this, 
		{ layout:'auto'
		//, layoutConfig: {align:'stretch'}
		, autoScroll: true
		, items: this.content
		, cls: 'GroupContainer'
		, unstyled: true
		});
		
		itasks.ttc.GroupContainer.superclass.initComponent.apply(this,arguments);
	},
	
	update: function(data) {
		this.updateElements(data.content);
	},
	
	updateElements: function(content){
		content = content.filter(function (val) { 
			if(val == "done" || val == "redundant") return false;
			else return true;
		});
		
		for(var i=0; i < content.length; i++){
			for(var j=i; j < this.items.length; j++){
				if(content[i].taskId == this.get(j).taskId) break;
			}
			
			for(var k=0; k < (j-i); k++){
				this.remove(i,true);
			}
			
			if(i < this.items.length){
				if (this.get(i).getXType() == content[i].xtype){
					this.get(i).update(content[i]);
				}else{
					//if not same xtype - completely replace tab contents
					this.remove(i);
					this.insert(i,content[i]);
				}
			}else{
				this.insert(j,content[i]);
			}
		}
		
		var trailing = this.items.length - content.length;
		for(var i=0; i<trailing; i++){
			this.remove(this.items.length-1,true);
		}

		this.doLayout();
	}
});

Ext.reg('itasks.ttc.group',itasks.ttc.GroupContainer);