Ext.ns('itasks.ttc');

itasks.ttc.TTCBase = Ext.extend(Ext.Panel, {
	
	initComponent : function() {
		this.initChildComponents();

		if(this.buildComponents) {
			this.buildComponents(this);	
		}			
		this.setChildComponentsWidth();
		Ext.apply(this, {
			taskUpdates : {},
			url: itasks.config.serverUrl + '/work/tab',
			unstyled: true,
			listeners: {tuichange: {fn:this.onTuiChange, scope: this}
			           ,tuiaction: {fn:this.onTuiAction, scope: this}
			           }
		});
		
		itasks.ttc.TTCBase.superclass.initComponent.apply(this,arguments);
	
		this.addEvents('tuievent','taskRedundant','taskDone');
		this.enableBubble('tuievent','taskRedundant','taskDone');
	},
	onTuiChange: function(taskId,name,value) {
		//Re-fire 'tuichange' events as 'tuievent' with the task number added
		this.fireEvent('tuievent',taskId,name,value);
		//Return such that 'higher' containers can't also try to handle this event.
		return false;	
	},
	onTuiAction: function(taskId,value) {
		//Re-fire 'tuiaction' events as 'tuievent' with name and task number added
		this.fireEvent('tuievent',taskId,'action',value);
		return false;
	},
	afterRender: function(){
		if (this.fadeIn) this.getEl().fadeIn({duration: itasks.ttc.TTC_FADE_DURATION});
	
		itasks.ttc.TTCBase.superclass.afterRender.apply(this,arguments);
		
		//Update references to the rendered components
		if(this.interactionpanel)
			this.interactionpanel = this.getComponent(0);
			
	},
	initChildComponents: function() {
		//Interaction panel, is not always used.
		this.interactionpanel = null;
	},
	rebuildComponents: function(data) {
		this.removeAll();
		this.initChildComponents();
		this.add(data.content);
		
		this.dirty = true;
		this.doLayout();
	},
	setChildComponentsWidth: function() {
		if (this.interactionpanel)
			this.setComponentWidth(this.interactionpanel);
	},
	update: function(data) {
		//Default update is to reconstruct the component
		this.menu = data.menu;
		this.rebuildComponents(data);
	},
	fadeOut: function(data) {
		if(data == "redundant"){
			msg = "The completion of this task is no longer required.<br />It has been removed. Thank you for your effort.";
			this.fireEvent("taskRedundant");
		}else{
			msg = "This task is completed. Thank you.";
			this.fireEvent("taskDone");
		}
		
		var par = this.findParentByType("itasks.ttc.parallel");
		if(par){
			var destroyCmp = this;
		}else{
			var destroyCmp = this.findParentByType("itasks.work");
		}
		
		var height = (this.interactionpanel ? this.interactionpanel.getHeight() : 0);
		this.removeAll();
		this.add({
			xtype: "itasks.ttc.finished",
			title: "Task completed",
			html: msg,
			destroyCmp: destroyCmp
		});
	
		this.doLayout();
	}
});