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
			xtype: 'itasks.ttc.common.description',
			cls: 'MessageDescription',
			description: data.description,
			headerButton: this.headerButton
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
			var wt = this.findParentByType(itasks.WorkPanel) || this.workPanel;
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
		
		var cls = 'ToolbarNoEnabledItems';
		if(enabledPresent)
			tb.removeClass(cls);
		else {
			tb.removeClass(cls);
			tb.addClass(cls);
		}
		
		var checkGroupOnly = function(item) {
			if(item.disabled)
				return true;
				
			if(item.name) {
				return item.name == '_group';
			} else if (item.getXType() != 'menuseparator') {
				var children =  item.items || item.menu.items;
				for(var i = 0; i < children.length; i++) {
					if (!checkGroupOnly(children.get(i)))
						return false;
				}
			}
			return true;
		};
		
		var cls = 'ToolbarGroupActionsOnly';
		if(checkGroupOnly(tb)) {
			tb.removeClass(cls);
			tb.addClass(cls);
		} else {
			tb.removeClass(cls);
		}
	}
});

Ext.ns('itasks.ttc.message');

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