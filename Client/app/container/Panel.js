Ext.define('itwc.container.Panel',{
	extend: 'Ext.panel.Panel',
	alias: 'widget.itwc_panel',
	requires: ['itwc.layout.container.Box'],
	autoScroll: true,
	
	//Default container config
	layout: 'itwc_box',
	halign: 'left',
	valign: 'top',
	direction: 'vertical',
	padding: 0,

	//Default dimensions
	width: 'flex',
	height: 'flex',
	minWidth: 'wrap',
	minHeight: 'wrap',

	initComponent: function() {
		
		//Create windows and link them back to this panel
		if(this.windows && this.windows.length) {
			var numWindows = this.windows.length,
				i;
			for(i = 0; i < numWindows; i++) {
				this.windows[i] = Ext.create('itwc.container.Window',this.windows[i]);
				//Create reference back to this panel
				this.windows[i].panelRef = this;
			}
			
		}
		//Set shrinkWrap using width & height values
		this.shrinkWrap = (this.width === 'wrap' ? 1 : 0) | (this.height === 'wrap' ? 2 : 0);

		this.layout = {type:'itwc_box', direction: this.direction, halign: this.halign, valign: this.valign, padding: 0};
		this.callParent(arguments);
	},
	onDestroy: function () {
		//Clean up the windows we created
		if(this.windows && this.windows.length) {
			var numWindows = this.windows.length,
				i;
			for(i = 0; i < numWindows; i++) {
				this.windows[i].destroy();
			}
		}
		this.callParent(arguments);
	},
	replaceWindow: function (index, def) {
		this.windows[index].destroy();
		this.windows[index] = Ext.create('itwc.container.Window',def);
		this.windows[index].panelRef = this;
	},
	insertWindow: function (index, def) {
		console.log("TODO insertWindow");
	},
	removeWindow: function (index) {
		console.log("TODO removeWindow");
	}
});
