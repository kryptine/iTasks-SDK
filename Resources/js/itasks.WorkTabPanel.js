/**
* Tab panel which shows a task a user is working on
*/

Ext.ns('itasks');

itasks.WorkTabPanel = Ext.extend(Ext.Panel, {

	updates: {}, 					//Dictionary with form updates
	inputs: [],
	state: undefined,				//The encoded state that is temporarily stored in the tab
	busy: false,					//Lock to prevent multiple requests at once
	debugPanel: undefined,			//An optional reference to a debug panel to find trace options
	applicationPanel: undefined,	//A reference to the application panel to find the session id
	
	lastFocus: undefined,			//The id of the last focused input
	firstBuffer: false,				//Use the first buffer panel (double buffering is used for rendering)
	prefixCounter: 0,				//Prefix counter 
	
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
					xtype: 'panel', //Task panel (no trace) 1
					border: false,
					cls: 'worktab-content',
					autoWidth: true,
					autoScroll: true
				},{
					xtype: 'panel', //Task panel (no trace) 2
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
						xtype: 'panel', //Task panel (trace) 1
						autoWidth: true,
						title: 'Task',
						cls: 'worktab-content'
					},{
						xtype: 'panel', //Task panel (trace) 2
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
			+ "<tr><th>From:</th><td>" + this.taskinfo.delegatorName + "</td><th>Priority:</th><td>" + itasks.util.formatPriority(this.taskinfo.priority) + "</td></tr>"
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
			var trace = (data.stateTrace != undefined || data.updateTrace != undefined || data.subtreeTrace != undefined);
						
			//Check for session errors.
			this.applicationPanel.checkSessionResponse(data);
			
			//Determine in which panel the new content must be put
			this.setNextContentPanel(trace);
			
			//Create content
			if (data.error != null) {
                this.autoClose(this.makeErrorMessage(data.error), 5);
            } else if(data.status == 'TaskFinished') { //Check if the task is done
				this.fireEvent('taskfinished', this.id);
				this.autoClose(this.makeFinishedMessage(), 5);
            } else if(data.status == 'TaskDeleted') {
                this.fireEvent('taskdeleted', this.id);
                this.autoClose(this.makeDeletedMessage(), 5);
            } else {	
				//Fill the content and trace panels
				this.setupContentPanel(trace, data);
				this.setupTracePanels(trace, data);
			}
			
			//Hide the current content panel and switch to new content
			this.switchContentPanels(trace);
				
			//Reset for new updates
			this.updates = {};
			this.inputs = data.inputs;
			this.state = data.state;
		}
		//Release the busy lock
		this.setBusy(false);
	},
	setNextContentPanel: function (trace) {
		if(trace) {
			this.contentPanel = this.getComponent(1).getComponent(2).getComponent(this.firstBuffer ? 0 : 1);
		} else {
			this.contentPanel = this.getComponent(1).getComponent(this.firstBuffer ? 0 : 1);
		}	
	},
	setupContentPanel: function (trace, data) {
		//Replace the content
		this.contentPanel.body.dom.innerHTML = data.html;

		//"ExtJS-ify" the inputs and attach event handlers
		var num = data.inputs.length;
		var forms = {};
		
		for(var i = 0; i < num; i++) {
			
			var inputname = data.inputs[i].formid + '-' + data.inputs[i].inputid;
			var inputid = data.prefix + inputname;
			var input = Ext.get(inputid);
			
			//Record the formid
			forms[data.inputs[i].formid] = true;
		
			//ExtJS-ify
			switch(data.inputs[i].type) {
				
				case "Int":
				case "Real":
				case "String":
					var value = input.getValue();
					var parent = input.parent();
					var next = input.next();
					
					//Replace
					input.remove();
					if(data.inputs[i].type == "String") {
						input = new Ext.form.TextField({
							id: inputid,
							name: inputname,
							value: value
						});
					} else {
						input = new Ext.form.NumberField({
							id: inputid,
							name: inputname,
							value: value,
							allowDecimals: (data.inputs[i].type == "Real"),
							decimalPrecision: 100, //Arbitrary limit
							style: "width: 5em"
						});
					}
					input.render(parent, next);
					
					//Event handlers
					if(data.inputs[i].updateon == "OnChange") {
						input.on("change", function (inp, newVal, oldVal) {
							this.addUpdate(inp.name, newVal);
							new Ext.util.DelayedTask().delay(150,this.refresh,this);
						},this);
					}
					if(data.inputs[i].updateon == "OnSubmit") {
						input.on("change", function (inp, newVal, oldVal) {
							this.addUpdate(inp.name, newVal);
						},this);
					}
					input.on("focus", function (inp) {
						this.lastFocus = inp.name;
					},this);
					
					break;
				case "Bool":
				case "Maybe":
				case "HtmlCheckbox":
					var checked = input.dom.checked;
					var parent = input.parent();
					var next = input.next();
					
					//Replace
					input.remove();
					input = new Ext.form.Checkbox({
						id: inputid,
						name: inputname,
						checked: checked
					});
					
					input.render(parent,next);
					
					//Attach event handlers
					if(data.inputs[i].updateon == "OnChange") {
						input.on("check", function (inp, checked) {
							this.addUpdate(inp.name, checked ? "checked" : "unchecked");
							new Ext.util.DelayedTask().delay(150,this.refresh,this);
						},this);
					}
					if(data.inputs[i].updateon == "OnSubmit") {
						input.on("check", function (inp, checked) {
							this.addUpdate(inp.name, checked ? "checked" : "unchecked");
						},this);
					}
					input.on("focus", function (inp) {
						alert("FOOCUSUUIS");
						this.lastFocus = inp.name;
					},this);
					
					break;
				case "HtmlButton":
					var label = input.dom.innerHTML;
					var parent = input.parent();
					var next = input.next();
					
					//Replace
					input.remove();
					input = new Ext.Button({
						id: inputid,
						name: inputname,
						text: label,
						style: "display: inline;"
					});
					
					input.render(parent,next);
					
					//Attach event handler
					input.on("click", function(but, e) {
						this.addUpdate(but.name, "click");
						this.refresh();
					},this);
					input.on("focus", function(inp) {
						this.lastFocus = inp.name;
					},this);
					break;
						
				//Default: Attach event handlers
				default:
					switch(data.inputs[i].updateon) {
						case "OnChange":
							input.on("change", function (e) {
								this.addUpdate(e.target.name,e.target.value);
								
								//Slightly delayed refresh. There could be click event right after this event.
								new Ext.util.DelayedTask().delay(150,this.refresh,this);
							},this);
							break;
						case "OnClick":
							input.on("click", function (e) {
								this.addUpdate(e.target.name,"click");
								this.refresh();
							},this);
							break;
						case "OnSubmit":
							input.on("change", function (e) {
								//Track changes, but don't send any data
								this.addUpdate(e.target.name,e.target.value);
							},this);
							break;
							
					}
					//Attach focus tracking handler
					input.on("focus", function (e) {
						this.lastFocus = e.target.name;
					},this);
			}
				
			//Refocus
			if(this.lastFocus == inputname) {
				input.focus(true,100);
			}
		}

		//Attach the submit handlers of the forms
		for(var formid in forms) {
			var form = Ext.get(data.prefix + formid);
			
			if(form != undefined) {
				//Cancel the form submit;
				form.dom.onsubmit = function() {return false;}
				
				//Attach our replacement event handler
				form.on("submit", function (e) {
					this.refresh();
				},this);
			}
		}		
	},
	setupTracePanels: function (trace, data) {

		if(!trace) {
			return;
		}
		
		var tabPanel = this.getComponent(1).getComponent(2);
				
		var statePanel = tabPanel.getComponent(2);
		if(data.stateTrace != undefined) {
			statePanel.getEl().dom.innerHTML = data.stateTrace;
			statePanel.enable();
		} else {
			statePanel.disable();
		}
		var updatePanel = tabPanel.getComponent(3);
		if(data.updateTrace != undefined) {
			updatePanel.getEl().dom.innerHTML = data.updateTrace;
			updatePanel.enable();
		} else {
			updatePanel.disable();
		}
		var subtreePanel = tabPanel.getComponent(4);
		if(data.subtreeTrace != undefined) {
			subtreePanel.getEl().dom.innerHTML = data.subtreeTrace;
			subtreePanel.enable();
		} else {
			subtreePanel.disable();
		}
	},
	switchContentPanels: function (trace) {
		
		var mainPanel = this.getComponent(1);
		
		if(trace) {
			mainPanel.layout.setActiveItem(2);
			
			var tabPanel = mainPanel.getComponent(2);
			
			tabPanel.setActiveTab(this.firstBuffer ? 0 : 1);
			tabPanel.unhideTabStripItem(this.firstBuffer ? 0 : 1);
			tabPanel.hideTabStripItem(this.firstBuffer ? 1 : 0);

		} else {
			mainPanel.layout.setActiveItem(this.firstBuffer ? 0 : 1);
		}
		//Toggle firstbuffer flag
		this.firstBuffer = !this.firstBuffer;
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
		
		//Add the prefix to the params
		params['prefix'] = 'tb' + (this.prefixCounter++) + '_';
		
		//Check if we need to request trace info
		if (this.debugPanel != undefined && this.debugPanel.traceEnabled()) {
				params['trace'] = 1;
		}
		//Add the session id
		params = this.applicationPanel.addSessionParam(params);
		
		//Send the data to the server
		Ext.Ajax.request({
			url: 'handlers/work/tab?taskid=' + this.id,
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