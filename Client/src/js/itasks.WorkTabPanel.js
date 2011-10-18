/**
* Tab panel which shows a task a user is working on
*/

Ext.ns("itasks");

itasks.WorkPanel = Ext.extend(itasks.RemoteDataPanel, {

	taskId: null,
	properties: null,
	
	debug: false,
	
	initComponent: function() {
		Ext.apply(this, {
			url: itasks.config.serviceUrl + "/json/tasks/" + this.taskId + "/tui",
			listeners:	{ tuichange: {fn:this.onTuiChange, scope: this}
						, tuiaction: {fn:this.onTuiAction, scope: this}
						}
		});		
		itasks.WorkPanel.superclass.initComponent.apply(this, arguments);
	
		this.addEvents("taskRedundant","taskDone","propertyChanged","afterUpdateContent");
		
		this.on("afterUpdateContent",function(){
			this.doLayout(false,true);
		});
	},
	onTuiChange: function(taskId,name,value) {
		if (!Ext.isDefined(this.params['editEvent'])) {
			this.params['editEvent'] = Ext.encode([taskId,name,value]);
			if (!Ext.isDefined(this.params['commitEvent'])) {
				// introduce slight delay to possibly send commit event in same request
				this.sendEvents.defer(100,this);
			}
		}	
	},
	onTuiAction: function(taskId,value) {
		if (!Ext.isDefined(this.params['commitEvent'])) {
			this.params['commitEvent'] = Ext.encode([taskId,value]);
			if (!Ext.isDefined(this.params['editEvent'])) {
				this.sendEvents();
			}
		}
	},
	sendEvents: function() {
		this.refresh(true);
		delete(this.params['editEvent']);
		delete(this.params['commitEvent']);
	},
	update: function(data) {
		
		//Store the timestamp of the current value
		this.timestamp = data.timestamp;
		
		//Update content
		if (Ext.isObject(data.content)) {
			data.content.xtype = 'itasks.tui.panel';
		}
		
		if(this.items.getCount() > 0) {
			//Recursively update window content
			this.getComponent(0).update(data.content);
		} else {
			//Build initial content
			this.add(data.content);
		}
	
		//Enable download / preview links
		var links   = Ext.query("a[name=x-form-document-link]");
		var plinks = Ext.query("a[name=x-form-document-preview-link]");
		
		for(var x=0; x < links.length; x++){
			var link = links[x];
			
			//link.href = Ext.urlAppend(link.href,'session='+itasks.app.session);
			link.name = "";
			
			for(var y=0; y < plinks.length; y++){				
				if(plinks[y].id == link.id){
					var plink = plinks[y];		

					plink.href="javascript:itasks.util.preview('"+link.href.replace( 'download','preview')+"')";
					plink.name = "";
				}
			}
		}
		
		if (data.warning)
			Ext.Msg.alert("Warning",data.warning);
	},
	fadeOut: function() {
		
		this.removeAll();
		this.add({
			xtype: "itasks.finished",
			title: "Task completed",
			html: "This task is completed or it's completion is no longer required.<br />Thank you for your effort."
		});
		this.doLayout();
	}
});

Ext.reg("itasks.work",itasks.WorkPanel);
