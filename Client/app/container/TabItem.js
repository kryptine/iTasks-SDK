Ext.define('itwc.container.TabItem',{
	extend: 'Ext.panel.Panel',
	alias: 'widget.itwc_tabitem',
	requires: ['itwc.layout.container.Box'],
	mixins: ['itwc.component.edit.Editable','itwc.container.HotkeyArea'], 

	//Default container config
	layout: 'itwc_box',
	halign: 'left',
	valign: 'top',
	direction: 'vertical',
	autoScroll: true,

	active: false,
	closable: false,
	border: false,
	
	initComponent: function() {
		var me = this;	

		me.layout = {type:'itwc_box', direction: me.direction, halign: me.halign, valign: me.valign};
		me.callParent(arguments);

		if(me.closeTaskId) {
			me.closable = true;
			me.addManagedListener(me,'beforeclose',me.onBeforeClose,me);
		}		
	},
	afterRender: function() {
		this.callParent(arguments);
		this.initHotkeys();
	},
	onDestroy: function () {
		this.destroyHotkeys();
		this.callParent(arguments);
	},
	onBeforeClose: function() {
		var me = this;

		if(me.closeTaskId) {
			me.viewport = me.findViewport();
			me.viewport.fireEvent('action',this.closeTaskId,'Close');
		}
		return false;
	},
	//Update operations
	setFocusTaskId: function (focusTaskId) {
		this.focusTaskId = focusTaskId;
	},
	setCloseTaskId: function (closeTaskId) {
		this.closeTaskId = closeTaskId;
	}
});
