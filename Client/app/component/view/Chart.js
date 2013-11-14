Ext.define('itwc.component.view.Chart',{
	extend: 'Ext.chart.Chart',
	alias: 'widget.itwc_view_chart',
	width: 400,
	height: 220,
	autoSize: false,
	
	
	initComponent: function() {
		var newData = [];
		var i = 0;
		this.theme = this.props.theme;
		this.axes = this.props.axes;
		this.data = this.props.data;
		this.series = this.props.series;
	
		for ( i=0; i<this.data.length;i++ )
		{
			for ( j=0;j<this.axes.length;j++ )
				if ( this.axes[j].type == "Numeric" )
					this.data[i][j] = parseInt(this.data[i][j]);
				
			newData.push({ x: this.data[i].x, y: this.data[i].y });
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
		
	}
});
