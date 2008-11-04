/**
* Tab panel which shows a task a user is working on
*/

Ext.ns('itasks');

itasks.WorkTabPanel = Ext.extend(Ext.Panel, {

	initComponent: function () {
		Ext.apply(this, {
			title: this.id,
			closable: true,
			html: this.id,
			baseCls: 'worktab',
			autoLoad: {
				url: 'handlers/work',
				method: 'GET',
				params: {
					taskid : this.id
				},
				scripts: false,
				callback: this.processTabData,
				scope: this
			}
		});
		
		itasks.WorkTabPanel.superclass.initComponent.apply(this, arguments);
	},
	
	processTabData: function (el,success,response,options) {
		if(success) {
			var data = Ext.decode(response.responseText);

			//Update the tab content
			el.dom.innerHTML = data.html;
		
			//Attach the event handlers
			//TODO
		}
	}
});

Ext.reg('itasks.worktab',itasks.WorkTabPanel);