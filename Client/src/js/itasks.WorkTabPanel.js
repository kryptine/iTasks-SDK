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
			title: "Loading...",
			closable: true,
			autoDestroy: true,
			iconCls: "icon-task",
			url: itasks.config.serviceUrl + "/json/tasks/" + this.taskId + "/tui",
			params: {session: itasks.app.session},
			listeners:	{ tuichange: {fn:this.onTuiChange, scope: this}
						, tuiaction: {fn:this.onTuiAction, scope: this}
						}
			
		});
			
		itasks.WorkPanel.superclass.initComponent.apply(this, arguments);
	
		this.addEvents("taskRedundant","taskDone","propertyChanged","afterUpdateContent");
		
		this.on("afterUpdateContent",function(){
			this.doLayout(false,true);
		});
		
		//Attach event handlers for the loading indicator
		this.on("remoteCallStart",function() {
			this.setIconClass("icon-waiting");
		},this);
		this.on("remoteCallEnd",function() {
			this.setIconClass("icon-task");
		},this);
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
	update: function(data,success,status) {
		if(!success) {
			if (status == 404) {
				// if process not found, fade out tab
				this.fadeOut();
			} else {
				// error other than "not found"
				Ext.Msg.alert('Error',"Error updating task: " + data);
			}
			return;
		} else if (data.tui == "done" || data.tui == "redundant") {
			this.fadeOut();
			return;
		}
		
		//Store the timestamp of the current value
		this.timestamp = data.timestamp;
		//Update content
		this.updateContent(data.tui);
	
		//Enable download / preview links
		var links   = Ext.query("a[name=x-form-document-link]");
		var plinks = Ext.query("a[name=x-form-document-preview-link]");
		
		for(var x=0; x < links.length; x++){
			var link = links[x];
			
			link.href = Ext.urlAppend(link.href,'session='+itasks.app.session);
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
	updateTitle: function(subject) {
		this.setTitle(Ext.util.Format.ellipsis(subject,10));
	},
	updateContent: function(content) {
		if (Ext.isObject(content)) {
			content.xtype = 'itasks.tui.panel';
		}
		if(this.items.getCount() > 0) {
			//Recursively update tab content
			var cur = this.getComponent(0);
			cur.update(content);
		} else {
			//Build initial content
			this.add(content);
			this.doLayout();
		}	
	},
	cancel: function(){
		var me = this;
		
		var doCancel = function(btn){	
			if(btn == "yes"){
				var url = itasks.config.serviceUrl + "/json/tasks/" + me.taskId + "/cancel";
				var params = {};
				var cb = function(data){
					if(data.success)
						me.fadeOut();
					else
						Ext.Msg.alert('Error','Failed to cancel task: '+data.error);
				};		
				me.remoteCall(url,params,cb);	
			}
		}
		
		Ext.Msg.confirm("Cancel Task","Are you sure you wish to cancel this task?",doCancel);
	},	
	fadeOut: function() {
		this.fireEvent("taskRedundant");
				
		this.removeAll();
		this.add({
			xtype: "itasks.finished",
			title: "Task completed",
			html: "This task is completed or it's completion is no longer required.<br />Thank you for your effort.",
			destroyCmp: this
		});
		this.doLayout();
	}
});

Ext.reg("itasks.work",itasks.WorkPanel);
