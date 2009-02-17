/**
* Tab panel which shows a task a user is working on
*/

Ext.ns('itasks');

itasks.WorkTabPanel = Ext.extend(itasks.RemoteDataPanel, {

	taskId: "",						//The taskid of the task done in this tab
	
	updates: {}, 					//Dictionary with form updates
	inputs: [],
	state: undefined,				//The encoded state that is temporarily stored in the tab
	trace: false,					//Request state information
	
	lastFocus: undefined,			//The id of the last focused input
	firstBuffer: false,				//Use the first buffer panel (double buffering is used for rendering)
	prefixCounter: 0,				//Prefix counter 
	
	contentPanel: undefined,		//A reference to the panel which is currently visible

	initComponent: function () {
	
		Ext.apply(this, {
			title: "...",
			closable: true,
			url: 'handlers/work/tab',
			params: {
				taskid: this.taskId,
				prefix: ""
			},
			layout: 'anchor',
			deferredRender: false,
			items: [{
				xtype: 'panel',
				anchor: '100%',
				height: 50,
				layout: 'anchor',
				items: [{
					xtype: 'panel',
					anchor: '0 0',
					baseCls: 'worktab-header'
					},{
					xtype: 'panel',
					anchor: '-50 100%',
					baseCls: 'worktab-header',
					html: "<div class=\"worktab-header-indicator\"></div>"
					}]			
			},{
				xtype: 'panel',
				anchor: '100% -50',
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
								this.updateForm();
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
		
		//Attach event handlers for the loading indicator
		this.on("remoteCallStart",function() {
			this.getComponent(0).getComponent(1).getEl().child(".worktab-header-indicator").setVisible(true);
		},this);
		this.on("remoteCallEnd",function() {
			this.getComponent(0).getComponent(1).getEl().child(".worktab-header-indicator").setVisible(false);
		},this);
	},
	setTrace: function (trace) {
		this.trace = trace;
	},
	makeFinishedMessage: function() {
		return "This task is completed. Thank you.";
	},
	makeDeletedMessage: function() {
		return "The completion of this task is no longer required.<br />It has been removed. Thank you for your effort.";
	},
	makeSuspendedMessage: function() {
		return "This task is temporarily suspended.<br />No work is needed at this moment.";
	},
	makeErrorMessage: function(msg) {
		return "<span class=\"error\">" + msg + "</span>";
	},
	update: function(data) {
		var trace = (data.stateTrace != undefined || data.updateTrace != undefined || data.subtreeTrace != undefined);
		
		//Determine in which panel the new content must be put
		this.setNextContentPanel(trace);
		//Update the header
		this.setupHeader(data);
		
		//Create content
		if (data.error != null) {
            this.autoClose(this.makeErrorMessage(data.error), 5);
        } else if(data.status == 'TaskFinished') { //Check if the task is done
			this.fireEvent('taskfinished', this.taskId);
			this.autoClose(this.makeFinishedMessage(), 5);
        } else if(data.status == 'TaskDeleted') {
            this.fireEvent('taskdeleted', this.taskId);
            this.autoClose(this.makeDeletedMessage(), 5);
       	} else if(data.status == 'TaskSuspended') {
       		this.fireEvent('tasksuspended', this.taskId);
       		this.setContent(this.makeSuspendedMessage());
        } else {
        	if(data.refresh) {
        		this.fireEvent('tasksuggestsrefresh',this.taskId);
        	}	
			//Fill the content and trace panels
			this.setupContentPanel(trace, data);
			this.setupTracePanels(trace, data);
		}
		
		//Hide the current content panel and switch to new content
		this.switchContentPanels(trace, data.prefix);
				
		//Reset for new updates
		this.updates = {};
		this.inputs = data.inputs;
		this.state = data.state;
	},
	setNextContentPanel: function (trace) {
		if(trace) {
			this.contentPanel = this.getComponent(1).getComponent(2).getComponent(this.firstBuffer ? 0 : 1);
		} else {
			this.contentPanel = this.getComponent(1).getComponent(this.firstBuffer ? 0 : 1);
		}	
	},
	setupHeader: function (data) {
		var html = "<div class=\"worktab-header-table\"><table>"
			+ "<tr><th>Subject:</th><td>" + data.subject + " (" + data.taskid + ")</td><th>Date:</th><td>" + itasks.util.formatDate(data.timestamp) + "</td></tr>"
			+ "<tr><th>Delegated by:</th><td>" + data.delegatorName + "</td><th>Priority:</th><td>" + itasks.util.formatPriority(data.priority) + "</td></tr>"
			+ "</table></div>";
			
		this.getComponent(0).getComponent(0).body.dom.innerHTML = html;
		this.setTitle(Ext.util.Format.ellipsis(data.subject,10));
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
				case "HtmlPassword":
					var value = input.getValue();
					var parent = input.parent();
					var next = input.next();
					
					//Replace
					input.remove();
					if(data.inputs[i].type == "String") {
						input = new Ext.form.TextField({
							id: inputid,
							name: inputid,
							inputName: inputname,
							value: value
						});
					} else if(data.inputs[i].type == "HtmlPassword") {
						input = new Ext.form.TextField({
							id: inputid,
							name: inputid,
							inputName: inputname,
							value: value,
							inputType: "password"
						});
					} else {
						input = new Ext.form.NumberField({
							id: inputid,
							name: inputid,
							inputName: inputname,
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
							this.addUpdate(inp.inputName, newVal);
							this.delayedUpdateForm();
						},this);
					}
					if(data.inputs[i].updateon == "OnSubmit") {
						input.on("change", function (inp, newVal, oldVal) {
							this.addUpdate(inp.inputName, newVal);
						},this);
					}
					input.on("focus", function (inp) {
						this.lastFocus = inp.inputName;
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
						inputName: inputname,
						checked: checked
					});
					
					input.render(parent,next);
					
					//Attach event handlers
					if(data.inputs[i].updateon == "OnChange") {
						input.on("check", function (inp, checked) {
							this.addUpdate(inp.inputName, checked ? "checked" : "unchecked");
							this.delayedUpdateForm();
						},this);
					}
					if(data.inputs[i].updateon == "OnSubmit") {
						input.on("check", function (inp, checked) {
							this.addUpdate(inp.inputName, checked ? "checked" : "unchecked");
						},this);
					}
					break;
				case "HtmlButton":
					var label = input.dom.innerHTML;
					var parent = input.parent();
					var next = input.next();
					
					//Replace
					input.remove();
					input = new Ext.Button({
						id: inputid,
						name: inputid,
						inputName: inputname,
						text: label,
						style: "display: inline;"
					});
					
					input.render(parent,next);
					
					//Attach event handler
					input.on("click", function(but, e) {
						this.lastFocus = but.inputName;
						this.addUpdate(but.inputName, "click");
						this.prepareParams();
						this.refresh();
					},this);
					input.on("focus", function(but) {
						this.lastFocus = but.inputName;
					},this);
					break;
					
				case "HtmlTextarea":
					var value = input.dom.innerHTML;
					var parent = input.parent();
					var next = input.next();
					
					//Replace
					input.remove();
					input = new Ext.form.TextArea({
						id: inputid,
						name: inputid,
						inputName: inputname,
						value: value,
						style: "width: 500px;"
					});
					input.render(parent,next);
					
					//Event handlers
					if(data.inputs[i].updateon == "OnChange") {
						input.on("change", function (inp, newVal, oldVal) {
							this.addUpdate(inp.inputName, newVal);
							this.delayedFormUpdate();
						},this);
					}
					if(data.inputs[i].updateon == "OnSubmit") {
						input.on("change", function (inp, newVal, oldVal) {
							this.addUpdate(inp.inputName, newVal);
						},this);
					}
					input.on("focus", function (inp) {
						this.lastFocus = inp.inputName;
					},this);
					
					break;
				case "HtmlDate":
					var value = input.getValue();
					var parent = input.parent();
					var next = input.next();
					
					//Replace
					input.remove();
					input = new Ext.form.DateField({
						id: inputid,
						name: inputid,
						inputName: inputname,
						value: value
					});
					input.render(parent,next);
					if(data.inputs[i].updateon == "OnChange") {
						input.on("change", function (inp, newVal, oldVal) {
							this.addUpdate(inp.inputName, newVal.format("m/d/Y"));
							this.delayedUpdateForm();
						},this);
					}
					if(data.inputs[i].updateon == "OnSubmit") {
						input.on("change", function (inp, newVal, oldVal) {
							this.addUpdate(inp.inputName, newVal.format("m/d/Y"));
						},this);
					}
					input.on("focus", function (inp) {
						this.lastFocus = inp.inputName;
					},this);
					break;
				case "HtmlTime":
					var value = input.getValue();
					var parent = input.parent();
					var next = input.next();
					
					//Replace
					input.remove();
					input = new Ext.form.TimeField({
						id: inputid,
						name: inputid,
						inputName: inputname,
						format: "H:i:s",
						value: value
					});
					input.render(parent,next);
					if(data.inputs[i].updateon == "OnChange") {
						input.on("change", function (inp, newVal, oldVal) {
							this.addUpdate(inp.inputName, newVal.format("H:i:s"));
							this.delayedUpdateForm();
						},this);
					}
					if(data.inputs[i].updateon == "OnSubmit") {
						input.on("change", function (inp, newVal, oldVal) {
							this.addUpdate(inp.inputName, newVal.format("H:i:s"));
						},this);
					}
					input.on("focus", function (inp) {
						this.lastFocus = inp.inputName;
					},this);
					break;
				case "HtmlRadiogroup":
					//Attach event handlers
					var k = 0;
					while(true) {
						var radio = Ext.get(inputid + '-' + k);
						if(radio == undefined) {
							break;
						}
						
						if(data.inputs[i].updateon == "OnChange") {
							radio.on("click",function(e) {
								this.addUpdate(e.target.name,e.target.value);
								this.delayedUpdateForm();
							},this);
						}
						if(data.inputs[i].updateon == "OnSubmit") {
							radio.on("click",function(e) {
								this.addUpdate(e.target.name,e.target.value);
							},this);
						}
						radio.on("focus", function (e) {
							this.lastFocus = e.target.name;
						},this);
						
						k++;
					}
					break;
				default:
					//Constructors and selects
					if(data.inputs[i].type.substr(0,5) == "CONS:" ||
					   data.inputs[i].type == "HtmlSelect") {
						
						var parent = input.parent();
						var next = input.next();
						
						input = new Ext.form.ComboBox({
							id: inputid,
							name: inputid,
							inputName: inputname,
							editable: false,
							triggerAction: "all",
							mode: "local",
							transform: input
						});
						
						input.render(parent,next);
						
						switch(data.inputs[i].updateon) {
							case "OnChange":
								input.on("select", function (inp) {
									this.addUpdate(inp.inputName,inp.value);
									this.delayedUpdateForm();
								},this);
								break;
							case "OnSubmit":
								input.on("change", function (inp) {
									//Track changes, but don't send any data
									this.addUpdate(inp.inputName,inp.value);
								},this);
								break;	
						}
						
						input.on("focus", function (inp) {
							this.lastFocus = inp.inputName;
						},this);
					}
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
	addUpdate: function (inputid, value) {
		this.updates[inputid] = value;
	},
	prepareParams: function () {
		this.params = this.updates;
		this.params['taskid'] = this.taskId;
		this.params['state'] = Ext.encode(this.state);
		this.params['prefix'] = 'tb' + (this.prefixCounter++) + '_';
		this.params['trace'] = this.trace ? 1 : 0;
	},
	updateForm: function () {
		this.prepareParams();
		this.refresh();
	},
	delayedUpdateForm: function() {
		new Ext.util.DelayedTask().delay(150,this.updateForm,this);
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
	setContent: function (msg) {
		this.contentPanel.body.dom.innerHTML = msg;
	},
	switchContentPanels: function (trace, prefix) {
		
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
		
		//Refocus
		var input = Ext.get(prefix + this.lastFocus)
		if(input != undefined) {
			input.focus();
		}
	},
	autoClose: function (msg, numSeconds) {
		if(numSeconds == 0 && this.ownerCt != undefined) {
			this.ownerCt.remove(this);
		} else {
			if(this.ownerCt != undefined) { //Only continue if we were not already closed manually
				this.contentPanel.body.dom.innerHTML = msg + '<br /><br />This tab will automatically close in ' + numSeconds + ' seconds...';		
		 		new Ext.util.DelayedTask().delay(1000,this.autoClose,this,[msg,numSeconds - 1]);
		 	}
		}
	}
});

Ext.reg('itasks.worktab',itasks.WorkTabPanel);