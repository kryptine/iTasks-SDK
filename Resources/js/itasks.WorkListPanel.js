/**
* This panel shows a list with work that a user has to do.
*/
Ext.ns('itasks');

itasks.WorkListPanel = Ext.extend(Ext.grid.GridPanel, {

	applicationPanel: undefined,
	
	workStore: new Ext.data.JsonStore({
		url: 'handlers/worklist',
		fields: [
			{name: 'taskid'},
			{name: 'priority'},
			{name: 'processname'},
			{name: 'subject'},
			{name: 'delegator'},
			{name: 'timestamp'}
		]
	}),	
	initComponent: function () {
		Ext.apply(this, {
			border: false,
			store: this.workStore,
			columns: [
				{id: 'taskid', header: 'Task ID', dataindex: 'taskid', width: 10 },
				{id: 'priority', header: 'Priority', dataindex: 'priority', renderer: itasks.util.formatPriority, width: 10 },
				{id: 'processname', header: 'Process', dataindex: 'processname', width: 25},
				{id: 'subject', header: 'Subject', dataIndex: 'subject', width: 35},
				{id: 'delegator', header: 'From', dataIndex: 'delegator', width: 10 },
				{id: 'timestamp', header: 'Date', dataIndex: 'timestamp', renderer: itasks.util.formatDate, width: 10}
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
	},
	setApplicationPanel: function(panel) {
		this.applicationPanel = panel;
	}, 
	/*
	* Return the taskid of the selected row
	*/
	getTaskId: function (index) {
		return this.workStore.getAt(index).data.taskid;
	},
	/*
	* Return all task information of the selected row
	*/
	getTaskInfo: function (index) {
		return this.workStore.getAt(index).data;
	},
	/*
	* Refresh the list
	*/
	refresh: function () {
		this.store.load({params: {session: this.applicationPanel.getSessionId()}});
	}
});

Ext.reg('itasks.worklist',itasks.WorkListPanel);
