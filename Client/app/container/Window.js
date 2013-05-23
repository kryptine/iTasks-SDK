Ext.define('itwc.container.Window',{
	extend: 'Ext.window.Window',
	alias: 'widget.itwc_window',
	requires: ['itwc.layout.container.Box'],
	mixins: ['itwc.component.edit.Editable','itwc.container.HotkeyArea'],

	autoShow: true,
	autoScroll: true,
	
	//Default container config
	//layout: 'itwc_box',
	halign: 'left',
	valign: 'top',
	direction: 'vertical',
	padding: 0,

	//Default dimensions
	
	width: 'flex',
	height: 'flex',
	minWidth: 'wrap',
	minHeight: 'wrap',

	viewport: null,
	
	initComponent: function() {
		
		//Set closable only if an action and task id are supplied
		if(this.closeTaskId) {
			this.closable = true;
		} else {
			this.closable = false;
		}
	
		//Set shrinkWrap using width & height values		
		//this.shrinkWrap = (this.width === 'wrap' ? 1 : 0) | (this.height === 'wrap' ? 2 : 0);
		
		this.layout = {type:'itwc_box', direction: this.direction, halign: this.halign, valign: this.valign, padding: this.padding};
		
		this.addManagedListener(this,'beforeclose',this.onBeforeClose,this);
		this.callParent(arguments);
	},
	onBeforeClose: function() {
		var me = this;
		
		me.viewport.fireEvent('action',me.closeTaskId,'Close');
		return false;	
	},
	afterRender: function() {
		this.callParent(arguments);
		this.initHotkeys();
	},
	onDestroy: function () {
		this.destroyHotkeys();
		this.callParent(arguments);
	}
});
