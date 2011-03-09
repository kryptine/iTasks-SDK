Ext.ns('itasks.ttc');

itasks.ttc.ParallelContainer = Ext.extend(itasks.ttc.TTCBase, {
	initComponent: function(){
		this.cls = 'TTCParallelControlContainer';
		
		itasks.ttc.ParallelContainer.superclass.initComponent.apply(this,arguments);	
	
		//Bubble the event to trigger viewing a task result
		this.addEvents('taskresult');
		this.enableBubble('taskresult');
		
		for(var i=0; i < this.content.length; i++) {
			return this.add(this.content[i]);
		}
	},
	buildComponents: function(data) {
	},
	update : function(data) {
		for(var i=0; i < data.content.length; i++) {
			this.get(i+2).update(data.content[i]);
		}
	}
});

Ext.reg('itasks.ttc.parallel',itasks.ttc.ParallelContainer);
