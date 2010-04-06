Ext.ns('itasks.ttc');

itasks.ttc.MessageContainer = Ext.extend(Ext.Panel, {

	initComponent : function() {
		
		this.buildComponents(this);
		this.tbar = this.content.tbar;
		
		delete this.content;
		
		Ext.apply(this,
		{ layout: 'anchor'
		, taskUpdates : {}
		, url: itasks.config.serverUrl + '/work/tab'
		, items: [this.descpanel,this.panel]
		, unstyled: true
		, autoScroll: true
		, cls: 'MessageContainer'
		});
		
		itasks.ttc.MessageContainer.superclass.initComponent.apply(this,arguments);
		
	},
	
	buildComponents: function(data){
		data.content.form.buttons = data.content.buttons;
		
		this.panel = {
			xtype: 'itasks.ttc.message.panel',
			items: data.content.form
		}
		
		this.descpanel = {
			xtype: 'itasks.ttc.message.description',
			html: data.description
		}
	},
	
	afterRender: function(){
		itasks.ttc.MessageContainer.superclass.afterRender.call(this,arguments);
		//attachTaskHandlers is moved to file 'TTCCommon.js'		
		itasks.ttc.common.attachTaskHandlers(this);
		
		var tb = this.getTopToolbar();
		this.setupToolbar(tb);
	},
		
	addUpdate: function(name, value) {
		this.taskUpdates[name] = value;
	},
	
	sendUpdates: function(delay) {
		if(delay) {
			new Ext.util.DelayedTask().delay(250,this.sendUpdates,this);
		} else {
			var wt = this.findParentByType(itasks.WorkPanel);
			if(!wt) return;
			
			wt.sendTaskUpdates(this.taskId,this.taskUpdates);
		
			this.taskUpdates = {};
		}
	},

	setupToolbar: function(tb) {
		tb.setVisible(tb.items.length > 0);
		
		var enabledPresent = false;
		for(var i = 0; i < tb.items.length; i++)
			if(!tb.get(i).disabled) {
				enabledPresent = true;
				break;
			}
			
		var cls = 'GroupToolbarNoEnabledItems';
		if(enabledPresent)
			tb.removeClass(cls);
		else {
			tb.removeClass(cls);
			tb.addClass(cls);
		}
	}
});

Ext.ns('itasks.ttc.message');

itasks.ttc.message.MessageDescription = Ext.extend(Ext.Panel,{
	initComponent : function(){
		Ext.apply(this,{		
			cls: 'task-description MessageDescription',
			unstyled: true,
		});
		
		itasks.ttc.message.MessageDescription.superclass.initComponent.apply(this,arguments);
	}
});

itasks.ttc.message.MessagePanel = Ext.extend(Ext.Panel, {

	initComponent : function(){
		Ext.apply(this,
		{ layout: 'fit'
		, unstyled: true
		, cls: 'MessagePanel'
		});
		
		itasks.ttc.message.MessagePanel.superclass.initComponent.apply(this,arguments);
	}

});

Ext.reg('itasks.ttc.message',itasks.ttc.MessageContainer);
Ext.reg('itasks.ttc.message.panel', itasks.ttc.message.MessagePanel);
Ext.reg('itasks.ttc.message.description', itasks.ttc.message.MessageDescription);