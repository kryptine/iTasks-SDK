/**
* This controller handles all events that have an effect on
* the state of server-side session task instances and updates the user interface 
* to reflect changes in those session task instance states
*/
Ext.define('itwc.controller.Controller',{
	extend: 'Ext.app.Controller',

	requires: ['itwc.container.Viewport'		//Top level container
			  ,'itwc.container.Container'		//Minimalist container that uses the itasks layout directives
			  ,'itwc.container.Panel'			//More configurable container with headers and stuff
			  ,'itwc.container.FieldSet'		//Logical grouping of components

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

			  ,'itwc.component.choice.Dropdown'	//A simple dropdown box for choosing from a fixed set
			  ,'itwc.component.choice.Grid'		//A grid from which you can select rows
			  ,'itwc.component.choice.Tree'		//A tree from which you can select nodes 
	
			  ,'itwc.component.action.ActionButton'		//A button that triggers an action event
			  ,'itwc.component.action.MenuButton'		//A button that opens a menu
			  ,'itwc.component.action.Menu',			//A menu wrapper
			  ,'itwc.component.action.ActionMenuItem'	//A menu item that triggers an action event

			  ,'itwc.component.misc.Splitter'	//Resize of adjacent components
			  ,'itwc.component.misc.Label'		//Label for standard forms
			  ,'itwc.component.misc.Icon'		//Icons with help text

			  ,'itwc.component.misc.Tab'		//A tab that can trigger focus events and close action events
			  ],

	init: function() {
		this.viewport = null;

		this.reqVersion = null;
	
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
		this.sendMessage();
	},
	//iTasks edit events
	onEdit: function(taskId, editorId, value) {
		console.log("Edit event", taskId, editorId, value);
	},
	//iTasks action events
	onAction: function(taskId, actionId) {
		console.log("Action event", taskId, actionId);
	},
	//iTasks focus events
	onFocus: function(taskId) {
		console.log("Focus event", taskId);
	},
	//Send a message to the server
	sendMessage: function() {
		var me = this,
			params = {};

        //Setup request parameters
        params['version'] = me.version;

        if(me.session)
            params['session'] = me.session;

		Ext.Ajax.request({
            url: '?format=json-gui',
            params: params,
            scripts: false,
            callback: me.receiveMessage,
            scope: me
        });

	},
	//Receive a message from the server
	receiveMessage: function(options,success,response) {
		var me = this,
			message;
		//Preprocess and check for errors
		//Transmission errors
		if(!success) {
			this.error("Request to server failed");
			return;
		}
		try {
			message = Ext.decode(response.responseText);
		} catch (SyntaxError) {
			this.error("Request from server malformed");
			return;
		}
		if (typeof message != 'object') {
			this.error("Request from server malformed");
            return;
        }
		//Server errors
		if(message.error) {
			this.error(message.error);
			return;
		}
		//Take action
        if(message.content) {
			me.fullUpdate(message.content);
		} else if(message.updates) {
			me.partialUpdate(message.updates);
		}
	},
	fullUpdate: function(viewportItems) {
		var me = this,
			viewport = me.viewport;

		//Close all windows
		Ext.WindowManager.each(function(w) {
			if (w.isXType('itwc_window')) {
				w.destroy();
			}
		});
		//Update viewport
		viewport.removeAll();
		viewport.add(viewportItems);
	},
	partialUpdate: function(updates) {
		console.log("Partial update requested");
	},
	error: function(e) {
        alert(e);
		//window.location = window.location;
	}
});
