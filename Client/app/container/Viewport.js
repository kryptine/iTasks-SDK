Ext.define('itwc.container.Viewport',{
	extend: 'Ext.container.Viewport',
	mixins: ['itwc.container.HotkeyArea'],
	requires: ['itwc.layout.container.Box'],

	padding: 0,
	direction: 'vertical',
	valign: 'middle',
	halign: 'center',

	windows: [],

	initComponent: function() {
		this.layout = {type:'itwc_box', direction: this.direction, halign: this.halign, valign: this.valign, padding: this.padding};
		this.callParent(arguments);
	},
	afterRender: function() {
		var me = this;
		
		me.callParent();
		me.initHotkeys();
	},
	onDestroy: function () {
		var me = this;

		me.destroyHotkeys();
		me.callParent(arguments);
	},
	getComponentByPath: function(path) {
		var me = this,
			numSteps,
			cmp = me,
			step, i, undef;

		if(path.length && path[0] === "w") { //Select window if the path starts with "w"
			path.shift();
			cmp = me.windows[path.shift()];
		}
		numSteps = path.length;

		for(i = 0; i < numSteps; i++) {
			step = path[i];
			
			if(step === "m") {
				cmp = cmp.getDockedComponent(0);
			} else {
				cmp = cmp.items && cmp.items.get && cmp.items.get(step);
			}
			if(!cmp)
				return undef;
		}
		return cmp;
	},
	setTitle: function(title) {
		document.title = title; //Set the html document title
	},
	addWindow: function (index, def) {
		var me = this;
		me.windows = me.windows || [];
		me.windows.splice(index,0,Ext.create('itwc.container.Window',def));
		me.windows[index].viewport = me;
	},
	removeWindow: function (index) {
		var me = this;
		me.windows = me.windows || [];
		me.windows[index].destroy();
		me.windows.splice(index,1);
	}
});
