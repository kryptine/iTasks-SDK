/**
* Tab panel which shows a task a user is working on
*/

Ext.ns('itasks');

itasks.WorkTabPanel = Ext.extend(Ext.Panel, {

	updates: {}, 			//Dictionary with form updates
	state: undefined,		//The encoded state that is temporarily stored in the tab

	debugPanel: undefined,	//An optional reference to a debug panel to find trace options
	
	initComponent: function () {
		Ext.apply(this, {
			title: this.makeTitle(),
			closable: true,
			layout: 'anchor',
			items: [{
				xtype: 'panel',
				id: 'worktab-header',
				anchor: '100%',
				height: 75,
				baseCls: 'worktab-header',
				html: this.makeHeader()
			},{
				xtype: 'panel',
				id: 'worktab-content',
				anchor: '100% -75',
				layout: 'card',
				autoScroll: true,
				activeItem: 0,
				items: [{
					xtype: 'panel',
					id: 'task-panel',
					ctCls: 'worktab-single',
					autoWidth: true,
					autoScroll: true
				},{
					xtype: 'tabpanel',
					id: 'trace-tabs',
					border: false,
					tabPosition: 'bottom',
					autoScroll: true,
					activeTab: 0,
					items: [{
						xtype: 'panel',
						id: 'trace-tab-task',
						ctCls: 'worktab-content',
						autoWidth: true,
						title: 'Task'
					},{
						xtype: 'panel',
						id: 'trace-tab-states',
						ctCls: 'worktab-content',
						autoWidth: true,
						title: 'States'
					},{
						xtype: 'panel',
						id: 'trace-tab-updates',
						ctCls: 'worktab-content',
						autoWidth: true,
						title: 'Updates'
					},{
						xtype: 'panel',
						id: 'trace-tab-subtree',
						ctCls: 'worktab-content',
						autoWidth: true,
						title: 'Sub task tree'
					}]
				},{	//This panel remains hidden but is used to load data into.
					xtype: 'panel',
					id: 'tab-buffer'
				}]
			}]
		});
		
		itasks.WorkTabPanel.superclass.initComponent.apply(this, arguments);
	},
	makeTitle: function() {
		return Ext.util.Format.ellipsis(this.taskinfo.subject,10);
	},
	makeHeader: function () {
		var html = '<table>';
		html += '<tr><th>Subject:</th><td>'+this.taskinfo.subject+'</td><th>Date:</th><td>'+itasks.util.formatDate(this.taskinfo.timestamp)+'</td></tr>';
		html += '<tr><th>TaskID:</th><td>'+this.taskinfo.taskid+'</td><th>Process:</th><td>'+this.taskinfo.processname+'</td></tr>';
		html += '<tr><th>For:</th><td>'+this.taskinfo.for+'</td><th>Priority:</th><td>'+itasks.util.formatPriority(this.taskinfo.priority)+'</td></tr>';
		html += '</table>';
		
		return html;
	},
	
	setDebugPanel: function (panel) {
		this.debugPanel = panel;
	},
	
	processTabData: function (el,success,response,options) {
			
		if(success) {
			var data = Ext.decode(response.responseText);
	
			//Clear the updates list
			this.updates = {};
			
			//Save the state
			this.state	= data.state;

			var contentPanel = this.getComponent('worktab-content');
			var taskPanel;
			
			//Check if trace information is available
	
			if(data.stateTrace || data.updateTrace || data.subtreeTrace) {
				var tracePanel = contentPanel.getComponent('trace-tabs');

				var statePanel = tracePanel.getComponent('trace-tab-states');
				if(data.stateTrace != undefined) {
					statePanel.show();
					statePanel.getEl().dom.innerHTML = data.stateTrace;
					statePanel.enable();
				} else {
					statePanel.disable();
				}
				var updatePanel = tracePanel.getComponent('trace-tab-updates');
				if(data.updateTrace != undefined) {
					updatePanel.show();
					updatePanel.getEl().dom.innerHTML = data.updateTrace;
					updatePanel.enable();
				} else {
					updatePanel.disable();
				}
				var subtreePanel = tracePanel.getComponent('trace-tab-subtree');
				if(data.subtreeTrace != undefined) {
					subtreePanel.show();
					subtreePanel.getEl().dom.innerHTML = data.subtreeTrace;
			
					subtreePanel.enable();
				} else {
					subtreePanel.disable();
				}
				
				tracePanel.setActiveTab(0);
				contentPanel.layout.setActiveItem(1);
				taskPanel = tracePanel.getComponent('trace-tab-task');
				
			} else {
				contentPanel.layout.setActiveItem(0);
				taskPanel = contentPanel.getComponent('task-panel');
			}
			
			//Update the tab content
			taskPanel.getEl().dom.innerHTML = data.html;
			
		
			//Attach the input event handlers
			var num = data.inputs.length;
			var forms = {};
			
			for(var i = 0; i < num; i++) {
				var inputid = data.inputs[i].formid + '-' + data.inputs[i].inputid;

				//Record the formid
				forms[data.inputs[i].formid] = true;
				
				//Attach the event 	
				switch(data.inputs[i].updateon) {
					case "OnChange":
						Ext.get(inputid).on("change", function (e) {
							this.addUpdate(e.target.id,e.target.value);
							this.refresh();
						},this);
						break;
					case "OnClick":
						Ext.get(inputid).on("click", function (e) {
							this.addUpdate(e.target.id,"click");
							this.refresh();
						},this);
						break;
					case "OnSubmit":
						Ext.get(inputid).on("change", function (e) {
							//Track changes, but don't send any data
							this.addUpdate(e.target.id,e.target.value);
						},this);
						break;
				}
			}
			//Attach the submit handlers of the forms
			for(var formid in forms) {
				var form = Ext.get(formid);
				
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
	refresh: function () {

		//Disable the "Loading..." indicator
		this.getUpdater().showLoadIndicator = false;
		
		//Add the state to the updates
		this.updates['state'] = Ext.encode(this.state);
		
		//Check if we need to request trace info
		var traceArgs = "";
		if (this.debugPanel != undefined) {
			if(this.debugPanel.traceStates()) {
				traceArgs += "&traceStates=1";
			}
			if(this.debugPanel.traceUpdates()) {
				traceArgs += "&traceUpdates=1";
			}
			if(this.debugPanel.traceSubTrees()) {
				traceArgs += "&traceSubTrees=1";
			}
		}
		
		//Send the data to the server
		this.getComponent('worktab-content').getComponent('tab-buffer').load({
			url: 'handlers/work?taskid=' + this.id + traceArgs,
			method: "POST",
			params: this.updates,
			scripts: false,
			callback: this.processTabData,
			scope: this,
		});
	}
});

Ext.reg('itasks.worktab',itasks.WorkTabPanel);