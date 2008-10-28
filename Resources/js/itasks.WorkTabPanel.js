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
				}
			}
		});
		
		itasks.WorkTabPanel.superclass.initComponent.apply(this, arguments);
	}
});

Ext.reg('itasks.worktab',itasks.WorkTabPanel);