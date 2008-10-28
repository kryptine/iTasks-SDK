/**
* This panel shows a list with work that a user has to do.
*/
Ext.ns('itasks');

itasks.WorkListPanel = Ext.extend(Ext.grid.GridPanel, {

	workStore: new Ext.data.JsonStore({
		url: 'handlers/worklist',
		fields: [
			{name: 'taskid'},
			{name: 'priority'},
			{name: 'processname'},
			{name: 'subject'},
			{name: 'for'},
			{name: 'timestamp'}
		]
	}),	
	initComponent: function () {
		Ext.apply(this, {
			border: false,
			store: this.workStore,
			columns: [
				{id: 'taskid', header: 'Task ID', dataindex: 'taskid', width: 10 },
				{id: 'priority', header: 'Priority', dataindex: 'priority', renderer: itasks.renderPriority, width: 10 },
				{id: 'processname', header: 'Process', dataindex: 'processname', width: 25},
				{id: 'subject', header: 'Subject', dataIndex: 'subject', width: 35},
				{id: 'for', header: 'For', dataIndex: 'for', width: 10 },
				{id: 'timestamp', header: 'Date', dataIndex: 'timestamp', renderer: itasks.renderDate, width: 10}
			],
			viewConfig: {
				forceFit: true,
				emptyText: 'There is no unfinished work.',
				deferEmptyText: false
			},
			autoExpandColumn: 'subject',
			enableHdMenu: false,
			stripeRows: true
		});
		
		itasks.WorkListPanel.superclass.initComponent.apply(this, arguments);
		
		//Load the data in the store
		this.workStore.load();
	},
	/*
	* Return the taskid of the selected row
	*/
	getTaskId: function (index) {
		return this.workStore.getAt(index).data.taskid;
	}
});

itasks.renderDate = function( timestamp) {
	return Date.parseDate(timestamp, "U").format("d M Y");
}
itasks.renderPriority = function(priority) {
	switch(priority) {
		case "LowPriority": return '<span style="color: green">Low</span>';
		case "NormalPriority": return '<span style="color: orange">Normal</span>';
		case "HighPriority": return '<span style="color: red">High</span>';
	}
	return priority;
}
Ext.reg('itasks.worklist',itasks.WorkListPanel);
