/**
* Tab panel which shows a task a user is working on
*/

Ext.ns('itasks');

itasks.WorkTabPanel = Ext.extend(Ext.Panel, {

	updates: {}, 					//Dictionary with form updates
	state: undefined,				//The encoded state that is temporarily stored in the tab
	busy: false,					//Lock to prevent multiple requests at once
	debugPanel: undefined,			//An optional reference to a debug panel to find trace options
	applicationPanel: undefined,	//A reference to the application panel to find the session id
	lastFocus: undefined,			//The id of the last focused input
	contentPanel: undefined,		//A reference to the panel which is currently visible

	initComponent: function () {
	
		Ext.apply(this, {
			title: this.makeTitle(),
			closable: true,
			layout: 'anchor',
			deferredRender: false,
			items: [{
				xtype: 'panel',
				anchor: '100%',
				height: 75,
				baseCls: 'worktab-header',
				html: this.makeHeader()				
			},{
				xtype: 'panel',
				anchor: '100% -75',
				layout: 'card',
				cls: 'worktab-container',
				deferredRender: false,
				tbar: [{
					text: 'Refresh task',
					iconCls: 'x-tbar-loading',
					listeners: {
						click: {
							scope: this,
							fn: function (btn) {
								this.refresh();
							}
						}
					}
				}],
				activeItem: 0,
				items: [{
					xtype: 'panel',
					border: false,
					cls: 'worktab-content',
					autoWidth: true,
					autoScroll: true
				},{
					xtype: 'tabpanel',
					border: false,
					tabPosition: 'bottom',
					autoScroll: true,
					deferredRender: false,
					layoutOnTabChange: true,
					activeTab: 0,
					items: [{
						xtype: 'panel',
						autoWidth: true,
						title: 'Task',
						cls: 'worktab-content'
					},{
						xtype: 'panel',
						autoWidth: true,
						title: 'States',
						cls: 'worktab-content'
					},{
						xtype: 'panel',
						autoWidth: true,
						title: 'Updates',
						cls: 'worktab-content'
					},{
						xtype: 'panel',
						autoWidth: true,
						title: 'Sub task tree',
						cls: 'worktab-content'
					}]
				}]
			}]
		});

		itasks.WorkTabPanel.superclass.initComponent.apply(this, arguments);
		
		//Set the initial content panel
		this.contentPanel = this.getComponent(1).getComponent(0);
	},

	makeTitle: function() {
		return Ext.util.Format.ellipsis(this.taskinfo.subject,10);
	},
	makeHeader: function () {
		return "<div class=\"worktab-header-table\"><table>"
			+ "<tr><th>Subject:</th><td>" + this.taskinfo.subject + "</td><th>Date:</th><td>" + itasks.util.formatDate(this.taskinfo.timestamp) + "</td></tr>"
			+ "<tr><th>TaskID:</th><td>" + this.taskinfo.taskid + "</td><th>Process:</th><td>" + this.taskinfo.processname + "</td></tr>"
			+ "<tr><th>For:</th><td>" + this.taskinfo.delegator + "</td><th>Priority:</th><td>" + itasks.util.formatPriority(this.taskinfo.priority) + "</td></tr>"
			+ "</table></div><div class=\"worktab-header-indicator\"></div>";
	},
	makeFinishedMessage: function() {
		return "This task is completed. Thank you.";
	},
	makeDeletedMessage: function() {
		return "The completion of this task is no longer required.<br />It has been removed. Thank you for your effort.";
	},
	makeErrorMessage: function(msg) {
		return "<span class=\"error\">" + msg + "</span>";
	},
	setDebugPanel: function (panel) {
		this.debugPanel = panel;
	},
	setApplicationPanel: function (panel) {
		this.applicationPanel = panel;
	},
	setBusy: function(busy) {
		this.busy = busy;
		this.getComponent(0).getEl().child(".worktab-header-indicator").setVisible(busy);
	},
	processTabData: function (el,success,response,options) {

		if(success) {
			var data = Ext.decode(response.responseText);
			
			//Check for session errors.
			this.applicationPanel.checkSessionResponse(data);
			
			//Clear the updates list
			this.updates = {};
			
			//Save the state
			this.state = data.state;

			var mainPanel = this.getComponent(1);
			var tracePanel = mainPanel.getComponent(1);
			
			//Check if trace information is available
	
			if(data.stateTrace != undefined || data.updateTrace != undefined || data.subtreeTrace != undefined) {
				
				mainPanel.layout.setActiveItem(1);

				this.contentPanel = tracePanel.getComponent(0);
				emptyPanel = mainPanel.getComponent(0);
				
				var statePanel = tracePanel.getComponent(1);
				if(data.stateTrace != undefined) {
					statePanel.getEl().dom.innerHTML = data.stateTrace;
					statePanel.enable();
				} else {
					statePanel.disable();
				}
				var updatePanel = tracePanel.getComponent(2);
				if(data.updateTrace != undefined) {
					updatePanel.getEl().dom.innerHTML = data.updateTrace;
					updatePanel.enable();
				} else {
					updatePanel.disable();
				}
				var subtreePanel = tracePanel.getComponent(3);
				if(data.subtreeTrace != undefined) {
					subtreePanel.getEl().dom.innerHTML = data.subtreeTrace;
					subtreePanel.enable();
				} else {
					subtreePanel.disable();
				}

				tracePanel.setActiveTab(0);
				
			} else {
				mainPanel.layout.setActiveItem(0);
				
				this.contentPanel = mainPanel.getComponent(0);
				emptyPanel = tracePanel.getComponent(0);
			}
			
			//Clear the panel which may contain content
			//of the previous request
			emptyPanel.body.dom.innerHTML = "";
			
			
			if (data.error != null) {
				this.autoClose(this.makeErrorMessage(data.error), 5);
			} else if(data.status == 'TaskFinished') { //Check if the task is done
				this.fireEvent('taskfinished', this.id);
				this.autoClose(this.makeFinishedMessage(), 5);
			} else if(data.status == 'TaskDeleted') {
				this.fireEvent('taskdeleted', this.id);
				this.autoClose(this.makeDeletedMessage(), 5);
			} else {
				//Update the tab content
				this.contentPanel.body.dom.innerHTML = data.html;
				
				//Attach the input event handlers
				var num = data.inputs.length;
				var forms = {};
				
				for(var i = 0; i < num; i++) {
					
					var inputid = data.inputs[i].formid + '-' + data.inputs[i].inputid;
					var input = Ext.get(inputid);
					
					//Record the formid
					forms[data.inputs[i].formid] = true;
					
					//Refocus
					if(this.lastFocus == inputid) {
						input.focus();
					}
					
					//Attach the event handlers
					switch(data.inputs[i].updateon) {
						case "OnChange":
							input.on("change", function (e) {
								this.addUpdate(e.target.id,e.target.value);
								
								//Slightly delayed refresh. There could be click event right after this event.
								new Ext.util.DelayedTask().delay(150,this.refresh,this);
							},this);
							break;
						case "OnClick":
							input.on("click", function (e) {
								this.addUpdate(e.target.id,"click");
								this.refresh();
							},this);
							break;
						case "OnSubmit":
							input.on("change", function (e) {
								//Track changes, but don't send any data
								this.addUpdate(e.target.id,e.target.value);
							},this);
							break;
					}
					//Attach focus tracking handler
					input.on("focus", function (e) {
						this.lastFocus = e.target.id;
					},this);
				}
	
				//Attach the submit handlers of the forms
				for(var formid in forms) {
					var form = Ext.get(formid);
					
					if(form != undefined) {
						//Cancel the form submit;
						form.dom.onsubmit = function() {return false;}
						
						//Attach our replacement event handler
						form.on("submit", function (e) {
							this.refresh();
						},this);
					}
				}
			}
		}
		//Release the busy lock
		this.setBusy(false);
	},
	
	addUpdate: function (inputid, value) {
		this.updates[inputid] = value;
	},

	refresh: function () {
		
		//If we are busy, return immediately
		if(this.busy) {
			return;
		} else {
			this.setBusy(true);
		}
		
		//The updates are the primary parameters
		var params = this.updates;
		
		//Add the state to the params
		params['state'] = Ext.encode(this.state);
		
		//Check if we need to request trace info
		if (this.debugPanel != undefined) {
			if(this.debugPanel.traceStates()) {
				params['traceStates'] = 1;
			}
			if(this.debugPanel.traceUpdates()) {
				params['traceUpdates'] = 1;
			}
			if(this.debugPanel.traceSubTrees()) {
				params['traceSubTrees'] = 1;
			}
		}
		//Add the session id
		params = this.applicationPanel.addSessionParam(params);
		
		//Send the data to the server
		Ext.Ajax.request({
			url: 'handlers/work?taskid=' + this.id,
			method: "POST",
			params: params,
			scripts: false,
			callback: this.processTabData,
			scope: this
		});
	},
	
	autoClose: function (msg, numSeconds) {
		if(numSeconds == 0) {
			if(this.ownerCt != undefined) {
				this.ownerCt.remove(this);
			}
		} else {
			if(this.ownerCt != undefined) { //Only continue if we were not already closed manually
				this.contentPanel.body.dom.innerHTML = msg + '<br /><br />This tab will automatically close in ' + numSeconds + ' seconds...';		
		 		new Ext.util.DelayedTask().delay(1000,this.autoClose,this,[msg,numSeconds - 1]);
		 	}
		}
	}
});

Ext.reg('itasks.worktab',itasks.WorkTabPanel);