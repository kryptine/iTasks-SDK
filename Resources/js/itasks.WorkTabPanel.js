/**
* Tab panel which shows a task a user is working on
*/

Ext.ns('itasks');

itasks.WorkTabPanel = Ext.extend(Ext.Panel, {

	updates: {}, 	//Dictionary with form updates
	state: "",		//The encoded state that is temporarily stored in the tab
	
	initComponent: function () {
		Ext.apply(this, {
			title: this.id,
			closable: true,
			html: this.id,
			baseCls: 'worktab',
			autoLoad: {
				url: 'handlers/work?taskid=' + this.id,
				method: 'GET',
				scripts: false,
				callback: this.processTabData,
				scope: this
			},
			autoScroll: true
		});
		
		itasks.WorkTabPanel.superclass.initComponent.apply(this, arguments);

	},
	
	processTabData: function (el,success,response,options) {
	
		
		if(success) {
			var data = Ext.decode(response.responseText);
	
			//Clear the updates list
			this.updates = {};

			//Update the tab content
			el.dom.innerHTML = data.html;
			
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
							this.sendData();
						},this);
						break;
					case "OnClick":
						Ext.get(inputid).on("click", function (e) {
							this.addUpdate(e.target.id,"click");
							this.sendData();
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
					this.sendData();
				},this);
			}
		}
	},
	addUpdate: function (inputid, value) {
		this.updates[inputid] = value;
	},
	sendData: function () {
		//Disable the "Loading..." indicator
		this.getUpdater().showLoadIndicator = false;
		
		//Send the data to the server
		this.load({
			url: 'handlers/work?taskid=' + this.id,
			method: "POST",
			params: this.updates,
			scripts: false,
			callback: this.processTabData,
			scope: this,
		});
	}
});

Ext.reg('itasks.worktab',itasks.WorkTabPanel);