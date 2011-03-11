Ext.ns('itasks.ttc');

itasks.ttc.ParallelContainer = Ext.extend(itasks.ttc.TTCBase, {
	initComponent: function(){
		this.cls = 'TTCParallelControlContainer';
		
		itasks.ttc.ParallelContainer.superclass.initComponent.apply(this,arguments);
		
		for(var i=0; i < this.content.length; i++) {
			this.add(this.content[i]);
		}
	},
	buildComponents: function(data) {
	},
	update : function(data) {
		if (data == "done" || data == "redundant"){
			this.fadeOut(data);
		} else {
			var curItemCount = this.items.getCount()-2;
			for(var i=0; i < curItemCount; i++) {
				this.get(i+2).update(data.content[i]);
			}
			
			var add = function(){
				for(var i=curItemCount; i < data.content.length; i++) {
					this.add(data.content[i]);
				}
				this.doLayout();
			};
			add.defer(itasks.ttc.TTC_FADE_DURATION * 1500,this);
		}
	}
});

Ext.reg('itasks.ttc.parallel',itasks.ttc.ParallelContainer);
