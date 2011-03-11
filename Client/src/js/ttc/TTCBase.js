Ext.ns('itasks.ttc');

itasks.ttc.TTCBase = Ext.extend(Ext.Panel, {
	
	initComponent : function() {
		this.initChildComponents();

		if(this.buildComponents) {
			this.buildComponents(this);	
		}			
		this.setChildComponentsWidth();
		Ext.apply(this, {
			layout: 'anchor',
			taskUpdates : {},
			url: itasks.config.serverUrl + '/work/tab',
			items: this.interactionpanel ? //Only add interactionpanel if defined in subclass
				[this.subjectpanel,this.descriptionpanel,this.interactionpanel] :
				[this.subjectpanel,this.descriptionpanel],
			unstyled: true,
			autoScroll: true,
			listeners: {tuichange: {fn:this.onTuiChange, scope: this}
			           ,tuiaction: {fn:this.onTuiAction, scope: this}
			           }
		});
		
		itasks.ttc.TTCBase.superclass.initComponent.apply(this,arguments);
	
		this.addEvents('tuievent');
		this.enableBubble('tuievent');
	},
	onTuiChange: function(name,value) {
		//Re-fire 'tuichange' events as 'tuievent' with the task number added
		this.fireEvent('tuievent',this.taskId,name,value);
		//Return such that 'higher' containers can't also try to handle this event.
		return false;	
	},
	onTuiAction: function(value) {
		//Re-fire 'tuiaction' events as 'tuievent' with name and task number added
		this.fireEvent('tuievent',this.taskId,'action',value);
		return false;
	},
	afterRender: function(){
		this.getEl().fadeIn({duration: itasks.ttc.TTC_FADE_DURATION});
	
		itasks.ttc.TTCBase.superclass.afterRender.call(this,arguments);		
		
		//Update references to the rendered components
		this.subjectpanel = this.getComponent(0);	
		this.descriptionpanel = this.getComponent(1);
		if(this.interactionpanel)
			this.interactionpanel = this.getComponent(2);
			
	},
	initChildComponents: function() {
		//Standard task header panel
		this.subjectpanel = {
			xtype: 'itasks.ttc.common.subject',
			subject: this.subject
		};
		//Standard task description panel
		this.descriptionpanel = {
			xtype: 'itasks.ttc.common.description',
			description: this.description,
			height: this.descriptionHeight
		};
		//Interaction panel, is not always used.
		this.interactionpanel = null;
	},
	rebuildComponents: function(data) {
		this.removeAll();
		this.initChildComponents();
		
		if(this.buildComponents)
			this.buildComponents(data);
		this.setChildComponentsWidth();
	
		this.subjectpanel = this.add(this.subjectpanel);
		this.descriptionpanel = this.add(this.descriptionpanel);
		this.interactionpanel = this.add(this.interactionpanel);
			
		this.doLayout();
	},
	setChildComponentsWidth: function() {
		this.setComponentWidth(this.subjectpanel);
		this.setComponentWidth(this.descriptionpanel);
		if (this.interactionpanel)
			this.setComponentWidth(this.interactionpanel);
	},
	setComponentWidth: function(component) {
		if (this.formWidth == 'FWFullWidth')
			delete component.width;
		else
			component.width = 720;
	},
	update: function(data) {
		//Default update is to reconstruct the component
		this.menu = data.menu;
		this.rebuildComponents(data);
	}
});