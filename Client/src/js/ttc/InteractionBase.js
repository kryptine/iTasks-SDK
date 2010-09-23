Ext.ns('itasks.ttc');

itasks.ttc.InteractionBase = Ext.extend(Ext.Panel, {

	initComponent : function() {
		this.buildComponents(this);
		this.tbar = this.content.tbar;
		delete this.content;
		
		Ext.apply(this,
		{ layout: 'anchor'
		, taskUpdates : {}
		, url: itasks.config.serverUrl + '/work/tab'
		, items: [this.subjectpanel,this.descriptionpanel,this.panel]
		, unstyled: true
		, autoScroll: true
		});
		
		itasks.ttc.InteractionBase.superclass.initComponent.apply(this,arguments);
	},
	
	afterRender: function(){
		itasks.ttc.InteractionBase.superclass.afterRender.call(this,arguments);
		//attachTaskHandlers is moved to file 'TTCCommon.js'		
		itasks.ttc.common.attachTaskHandlers(this);
		
		this.setupToolbar();
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
	
	setupToolbar: function() {
		var tb = this.getTopToolbar();
		
		tb.setVisible(tb.items.length > 0);
		
		var enabledPresent = false;
		for(var i = 0; i < tb.items.length; i++) {
			if(!tb.get(i).disabled) {
				enabledPresent = true;
				break;
			}
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
				
			if(Ext.isBoolean(item.topGroupAction)) {
				return item.topGroupAction;
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
		
		itasks.ttc.common.attachTaskHandlers(this);
		itasks.ttc.common.setupHotkeys(tb, this);
	}
});