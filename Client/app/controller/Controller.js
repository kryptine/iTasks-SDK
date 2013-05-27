/**
* This controller handles all events that have an effect on
* the state of server-side session task instances and updates the user interface 
* to reflect changes in those session task instance states
*/
Ext.define('itwc.controller.Controller', {
	extend: 'Ext.app.Controller',

	requires: ['itwc.container.Container'		//Minimalist container that uses the itasks layout directives
			  ,'itwc.container.Panel'			//More configurable container with headers and stuff
			  ,'itwc.container.TabSet'			//Container for grouping a set of panels as a stack of tabs
			  ,'itwc.container.TabItem'			//Container that is one tab in a set.
			  ,'itwc.container.FieldSet'		//Logical grouping of components
			  ,'itwc.container.Window'			//Floating window

			  ,'itwc.container.Tasklet'			//Tasklet
			  ,'itwc.container.TaskletPlaceholder'
  
			  ,'itwc.component.view.String'		//Strings with html escaping
			  ,'itwc.component.view.HTML'		//Raw HTML
			  ,'itwc.component.view.Checkbox'	//Checkbox for booleans
			  ,'itwc.component.view.Slider'		//Slider for bounded values
			  ,'itwc.component.view.Progress'	//Progress bar
			  ,'itwc.component.view.Document'	//Viewing of downloadable documents

			  ,'itwc.component.edit.String'		//An editor for strings 
			  ,'itwc.component.edit.Int'		//An editor for integer values 
			  ,'itwc.component.edit.Decimal'	//An editor for decimal values 
			  ,'itwc.component.edit.Checkbox'	//An editor for boolean values 
			  ,'itwc.component.edit.Note'		//An editor for notes
			  ,'itwc.component.edit.Date'		//An editor for dates
			  ,'itwc.component.edit.Time'		//An editor for times
			  ,'itwc.component.edit.Password'	//An editor for passwords 
			  ,'itwc.component.edit.Slider'		//A slider for bounded values
			  ,'itwc.component.edit.Document'	//An editor for uploading documents
			  ,'itwc.component.edit.EditButton'	//A button that fires edit events
			  ,'itwc.component.edit.GoogleMap'	//An embedded Google map
			  ,'itwc.component.edit.Code'		//A source code text editor
			  
			  ,'itwc.component.choice.Dropdown'	     //A simple dropdown box for choosing from a fixed set
			  ,'itwc.component.choice.RadioGroup'    //A set of radio buttons with labels
			  ,'itwc.component.choice.CheckboxGroup' //A set of checkboxes with labels
			  ,'itwc.component.choice.Grid'		     //A grid from which you can select rows
			  ,'itwc.component.choice.Tree'		     //A tree from which you can select nodes 
	
			  ,'itwc.component.action.ActionButton'		//A button that triggers an action event
			  ,'itwc.component.action.MenuButton'		//A button that opens a menu
			  ,'itwc.component.action.Menu'			//A menu wrapper
			  ,'itwc.component.action.ActionMenuItem'	//A menu item that triggers an action event
			  ,'itwc.component.action.SubMenuItem'		//A menu item that leads to a sub menu 

			  ,'itwc.component.misc.Splitter'	//Resize of adjacent components
			  ,'itwc.component.misc.Label'		//Label for standard forms
			  ,'itwc.component.misc.Icon'		//Icons with help text

			  ,'itwc.component.misc.Tab'		//A tab that can trigger focus events and close action events
			  ],

	// for tasklet and client side execution support
	tasklets: {}, 				// tasklet instance id -> tasklet
	taskletControllers: {},		// instanceNo          -> tasklet if controllerFunc is avaliable

	// central task event queue
	taskEvents: [],
	nextSendEventNo: 0,
	flushingTaskEvents: false,

	init: function() {
		this.viewport = null;

		this.refresher = new Ext.util.DelayedTask(this.onAutoRefresh,this);
		this.control({
			'viewport': {
				render: this.onViewportReady,
				edit: this.onEdit,
				action: this.onAction,
				focus: this.onFocus
			}
		});
		//Scary global reference for Tasklets
		controller = this;
	},
	onViewportReady: function(viewport) {
		//Keep reference to server
		this.viewport = viewport;

		//Sync with server for the first time
		this.queueTaskEvent({});
	},
	//iTasks edit events
	onEdit: function(taskId, editorId, value) {
		
		// Client side execution hook!
		var instanceNo = taskId.split("-")[0];
		if(this.taskletControllers[instanceNo] != null){
		
			controllerWrapper(
					this.taskletControllers[instanceNo].iid,
					this.taskletControllers[instanceNo].controllerFunc, 
					taskId, "edit", editorId, value);
		
		}else{	// Normal case (not a tasklet)
			
			this.sendEditEvent(taskId, editorId, value);
		}
	},
	sendEditEvent: function(taskId, editorId, value){

		var me = this,
			params = {editEvent: Ext.encode([taskId,editorId,value])};
		
		//me.sendMessage(params); //TEMPORARILY DUMB WITHOUT QUEUE AND TRACKING	
		me.queueTaskEvent(params);
	},
	//iTasks action events
	onAction: function(taskId, actionId) {
	
		// Client side execution hook!
		var instanceNo = taskId.split("-")[0];
		if(this.taskletControllers[instanceNo] != null){
			
			controllerWrapper(
					this.taskletControllers[instanceNo].iid,
					this.taskletControllers[instanceNo].controllerFunc, 
					taskId, "commit", actionId);
					
		}else{	// Normal case (not a tasklet)
	
			var me = this,
				params = {actionEvent: Ext.encode([taskId,actionId])};

			me.queueTaskEvent(params);
		}
	},
	//iTasks focus events
	onFocus: function(taskId) {
		var me = this,
			params = {focusEvent: Ext.encode(taskId)};
		me.queueTaskEvent(params);
	},
	//Auto refresh event (triggered by tasks with an expiresIn value)
	onAutoRefresh: function () {
		var me = this;
		if(me.taskEvents.length == 0) {//Only send empty event if we are idly waiting for events
			me.queueTaskEvent({});
		}
	},
	//Queue a task event for processing
	queueTaskEvent: function(eventData) {
		var me = this;
		me.taskEvents.push([me.nextSendEventNo++,eventData]);
		me.flushTaskEvents();
	},
	//Flush the current task events to the server for processing
	//If the force option is given, also flush an empty queue, effectively asking for a refresh
	flushTaskEvents: function() {
		var me = this,
			params = {},
			event;
		if(!me.flushingTaskEvents && me.taskEvents.length) {
			//Send events one at a time for now...
			event = me.taskEvents.shift();
			Ext.apply(params,event[1]);
			params['version'] = event[0];

        	if(me.session) {
           		params['session'] = me.session;
			}

			me.flushingTaskEvents = true;
			Ext.Ajax.request({
				url: '?format=json-gui',
				params: params,
				scripts: false,
				callback: me.receiveTaskUpdates,
				scope: me
			});
		}
	},
	receiveTaskUpdates: function(options,success,response) {
		var me = this,
			message;
		//Preprocess and check for errors
		//Transmission errors
		if(!success) {
			me.error("Request to server failed");
			return;
		}
		try {
			message = Ext.decode(response.responseText);
		} catch (SyntaxError) {
			me.error("Request from server malformed");
			return;
		}
		if (typeof message != 'object') {
			me.error("Request from server malformed");
            return;
        }
		//Server errors
		if(message.error) {
			me.error(message.error);
			return;
		}
		//Update session
		me.session = message.session;

		//Update user interface
        if(message.content) {
			me.fullUpdate(message.content);
		} else if(message.updates) {
			me.partialUpdate(message.updates);
		}
		//Schedule automatic refresh when an expiration time is set
		if(Ext.isNumber(message.expiresIn)) {
			me.refresher.delay(message.expiresIn);
		}

		//Send remaining messages
		me.flushingTaskEvents = false;
		me.flushTaskEvents();
	},
	fullUpdate: function(viewportDef) {
		var me = this,
			viewport = me.viewport;
	
		//Update the main window title instead of the viewport panel	
		document.title = viewportDef.title ? viewportDef.title : '';
		
		//Update viewport
		viewport.removeAll();
		viewport.add(viewportDef.items);
	},
	partialUpdate: function(updates) {
		var me = this,
			numUpdates = updates.length,
			update, 
			cmp, operations, numOperations, operation, i, j;
			
		for(i = 0; i < numUpdates; i++) {
			update = updates[i];
			//try {
				if(cmp = me.viewport.getComponentByPath(update.path)) {
					operations = update.operations;
					numOperations = operations.length;

					//If multiple operations need to be done on the same component, don't layout in between
					if(numOperations > 1 && cmp.doLayout) { 
						cmp.suspendLayout = true;
					}
					for(j = 0; j < numOperations; j++) {
						operation = operations[j];	

						//Try to call the update method
						if(cmp && typeof cmp[operation.method] == 'function') {
							cmp[operation.method].apply(cmp,operation.arguments);
						} else {
							me.error("Can't apply " + operation.method + " to " + cmp.getId() + " (" + cmp.getXType() + ")");
						}
					}
					if(cmp.suspendLayout) {
						cmp.suspendLayout = false;
						cmp.doLayout();
					}
				} else {
					me.error("Could not find user interface component at location " + update.path);
				}
			//} catch (e) {
			//	me.error("Failed to update user interface " + e);
			//}
		}
	},
	error: function(e) {
        alert(e);
		//window.location = window.location;
	},
	warn: function(w) {
		if(console && console.log) {
			console.log("Warning:",w);
		}
	}
});
