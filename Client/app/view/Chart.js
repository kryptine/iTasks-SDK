Ext.define('itwc.component.view.Chart',{
	extend: 'Ext.chart.Chart',
	alias: 'widget.itwc_view_chart',
	width: 400,
	height: 220,
	autoSize: true,
	
	
	initComponent: function() {
		var newData = [];
		var i = 0;
	
		for ( i=0; i<this.data.length;i++ )
		{
			for ( j=0;j<this.axes.length;j++ )
				if ( this.axes[j].type == "Numeric" )
					this.data[i][j] = parseInt(this.data[i][j]);
				
			newData.push({ x: this.data[i][0], y: this.data[i][1] });
		}
		this.model = Ext.define('Simple', {
			extend: 'Ext.data.Model',
			fields: ['x', 'y']
		});
		this.dataStore = Ext.create('Ext.data.Store', {
			model: 'Simple',
			data: newData
		});
		this.store = this.dataStore;
		this.callParent(arguments);
	},
	afterRender: function() {
		var me = this;
		
		me.callParent(arguments);
	},
	apiLoaded: function() {
	},
	apiLoaded: function() {
	},
    afterComponentLayout: function() {
    	this.callParent(arguments);
    },
	setValue: function(value) {
	},
	selfUpdate: function(def) {
		var me = this;
	},
	onDestroy: function() {
		
	}
});
