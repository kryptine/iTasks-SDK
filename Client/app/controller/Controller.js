Ext.define('itasks.controller.Controller',{
	extend: 'Ext.app.Controller',
	requires: [
		'itasks.component.edit.String',
		'itasks.component.show.String',
		'itasks.component.edit.Char',
		'itasks.component.show.Char',
		'itasks.component.edit.Bool',
		'itasks.component.show.Bool',
		'itasks.component.edit.Int',
		'itasks.component.show.Int',
		'itasks.component.edit.Real',
		'itasks.component.show.Real',
		'itasks.component.edit.Note',
		'itasks.component.show.Note',
		'itasks.component.edit.Date',
		'itasks.component.show.Date',
		'itasks.component.edit.Time',
		'itasks.component.show.Time',
		'itasks.component.edit.Password',
		'itasks.component.show.Password',
		'itasks.component.edit.Currency',
		'itasks.component.show.Currency',
		'itasks.component.Combo',
		'itasks.component.Tree',
		'itasks.component.Grid',
		'itasks.component.Icon',
		'itasks.component.Button',
		'itasks.component.MenuButton',
		'itasks.component.MenuItem',
		'itasks.component.Html',
		'itasks.container.Panel',
		'itasks.container.Container',
		'itasks.container.Window',
		'itasks.container.RadioChoice',
		'itasks.container.CheckChoice',
		'itasks.container.TabContainer',
		'itasks.container.TabItem',
		'itasks.container.BorderContainer',
		'itasks.container.BorderItem',
		'itasks.container.ListContainer',
		'itasks.container.ListItem'
	],

	init: function () {

		this.version = 0;
		this.sessionId = null;
		this.editEvent = null;
		this.commitEvent = null;

		this.viewport = null;

		//Check when the viewport is rendered
		this.control('viewport',{
			render: this.onViewportRendered,
			edit: this.onEdit,
			commit: this.onCommit
		});
		//Control the menus
		this.control('itasks_menu_item', {
			commit: this.onCommit
		});
		this.control('.itasks_menu_button', {
			commit: this.onCommit
		});
	},
	//Once, the viewport is rendered we can load the initial
	//user interface definition
	onViewportRendered: function(viewport) {
		document.title = 'Running...';
		this.viewport = viewport;
		this.pollServer();
	},
	onEdit: function(taskId,name,value) {
		this.editEvent = [taskId,name,value];
		this.pollServer();
	},
	onCommit: function(taskId,name) {
		this.commitEvent = [taskId,name];
		this.pollServer();
	},

	// SERVER CONTROL PROCESSING

	//Interface definition loading / updating
	pollServer: function() {
	
		//Send additional info, namely session and lastsync	
		var params = {};
		if(this.sessionId)
			params['session'] = this.sessionId;

		//Send gui version number
		params['version'] = this.version;

		//If event and/or commit data is available, add it to the params
		if(this.editEvent != null) {
			params['editEvent'] = Ext.encode(this.editEvent);
			this.editEvent = null;
		}
		if(this.commitEvent != null) {
			params['commitEvent'] = Ext.encode(this.commitEvent);
			this.commitEvent = null;
		}

		Ext.Ajax.request({
			url: '?format=json-gui',
			params: params,
			scripts: false,
			callback: this.processServerMessage,
			scope: this
		});
	
		//Increase version number
		this.version++;
	},
	//Process server interface definitions
	//@private
	processServerMessage: function(options,success,response) {
		var message;

		//Preprocess and check for errors
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
		
		if(message.error) {
			this.error(message.error);
			return;
		}

		//Update session attribute
		if(message.session) {
			this.sessionId = message.session;
		}
		//Reload entire interface
		if(message.content) {
			this.viewport.suspendLayout = true;
			
			//Remove old content
			Ext.WindowManager.each(function(w) {
				if (w.isXType('itasks_window')) {
					w.destroy();
				}
			});
			this.viewport.removeAll();
			
			//Add new content
			this.viewport.add(message.content);
			this.viewport.suspendLayout = false;
			this.viewport.doLayout();
			
			//Enable events
			var cmps = Ext.ComponentQuery.query('[editable=true]');
			var cmp, i;
		
			for(i = 0;  i < cmps.length; i++) {
				cmp = cmps[i];
				cmp.startSyncEdits();
			}
		
			return;
		}
		//Update existing interface
		if(message.updates) {
			var i, update, updateCount = message.updates.length, target;
	
			for(i = 0; i < updateCount; i++) {
				update = message.updates[i];
			
				try {
					target = this.findChildByPath(update.path, this.viewport);
				
					if(target && typeof target[update.method] == 'function') {
						target[update.method].apply(target,update.arguments);
					} else {
						//If replace is not defined as function, try remove followed by add
						if(update.method == 'replace' && typeof target['remove'] == 'function' && typeof target['insert'] == 'function') {
							target.remove(update.arguments[0]);
							target.insert(update.arguments[0],update.arguments[1]);
						} else {
							if(!target) {
								this.error("Could not find user interface component at location " + update.path);
							} else {
								this.error("Can't apply " + update.method + " to " + target.getId() + " (" + target.getXType() + ")");
								this.error(update.arguments);
							}
						}
					}
					
				} catch (e) {
					this.error("Failed to update user interface " + e);
				}
			}
			//Enable events
			var cmps = Ext.ComponentQuery.query('[editable=true]');
			var cmp, i;
		
			for(i = 0;  i < cmps.length; i++) {
				cmp = cmps[i];
				cmp.startSyncEdits();
			}

			return;
		}
		//Shut down application
		if(message.done) {
			this.viewport.removeAll();
			document.title = 'Stopped.';
			return;
		}
	},
	findChildByPath: function(path, cmp) {

		var steps = path.split('/'),
			step,
			child = cmp,
			numSteps = steps.length,
			i, undefinedValue;
			
		if(path == "")
			return child;
		
		for(i = 0; i < numSteps; i++) {
			step = steps[i];
		
			if(step === "m") {
				child = child.getDockedComponent(0);
					if(!child)
						return undefinedValue;
			} else {
				step = parseInt(step);
				if(child.managing) {
					child = child.managed[step];
					if(!child)
						return undefinedValue;
				} else if(child.items && child.items.get) {
					child = child.items.get(step);
					if(!child)
						return undefinedValue;
				} else {
					return undefinedValue;
				}
			}
		}
		return child;
	},
	error: function(e) {
		alert(e);
		window.location = window.location;
	}
});
