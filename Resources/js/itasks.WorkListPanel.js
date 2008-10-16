/**
* This panel shows a list with work that a user has to do.
*/
Ext.ns('itasks');

itasks.WorkListPanel = Ext.extend(Ext.grid.GridPanel, {

	workStore: new Ext.data.JsonStore({
		url: 'handlers/worklist',
		fields: [
			{name: 'for'},
			{name: 'subject'}
		]
	}),
	
	initComponent: function () {
		Ext.apply(this, {
			border: false,
			store: this.workStore,
			columns: [
				{id: 'for', header: 'For', dataIndex: 'for', width: 20 },
				{id: 'subject', header: 'Subject', dataIndex: 'subject', width: 80}
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
	}

});

Ext.reg('itasks.worklist',itasks.WorkListPanel);
