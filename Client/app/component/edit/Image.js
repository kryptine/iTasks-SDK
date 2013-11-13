Ext.define('itwc.component.edit.Image',{
	extend: 'Ext.draw.Component',
	alias: 'widget.itwc_edit_image',
	mixins: ['itwc.component.edit.Editable'],

	minWidth: 400,
	minHeight: 220,

	autoSize: true,
	
	imageItems : [],
	animationTimer: 0,
	enableKeyEvents: true,
	selected : [],
	initComponent: function() {
		var i = 0;
		this.items = [];

		for ( i=0;i<this.imageItems.length;i++ )
		{
			var shp = this.imageItems[i];
			if ( shp.type != "line" ){
				this.items.push(shp);
				this.items[i].identifier = shp.identifier;
				this.items[i]["stroke-width"] = shp.strokeWidth;
				
				for ( j=0;j<this.selected.length;j++ )
					if ( this.selected[j] == this.items[i].identifier )
						this.items[i].fill = "yellow";
			}
			else{
				this.addLine(shp.x, shp.x2, shp.y, shp.y2, shp.fill, shp.drawArrow, shp.stroke, shp.strokeWidth, shp.identifier);
			}
			
		}
		
		Ext.EventManager.on(window, 'keydown', function(e) {
		
			if (e.getKey() == 16) {
				this.shiftKeyPressed = true; // a flag
				return;
			}
			if (e.getKey() != e.ENTER && (this.shiftKeyPressed == undefined || (this.shiftKeyPressed == false))) {
				// Submit form
				e.stopEvent();
			}
		}, this);
		
		Ext.EventManager.on(window, 'keyup',  function(e) {
			if (e.getKey() == 16) {
				this.shiftKeyPressed = false;
			}
		}, this);

		this.callParent(arguments);
		this.initEditable();
	},
	addLine: function(x1, x2, y1, y2, fill, drawArrow, stroke, strokeWidth, id){
		var me = this;
		var angle = Math.atan2(x1-x2,y2-y1);
		var size = 1;
		angle = (angle / (2 * Math.PI)) * 360;
		var arrowPath = "M" + x2 + " " + y2 + " L" + (x2 - size) + " " + (y2 - size) + " L" + (x2 - size) + " " + (y2 + size) + " L" + x2 + " " + y2;
		var linePath = "M" + x1 + " " + y1 + " L" + x2 + " " + y2;
		
		me.items.push(
				{
					type: "path",
					path: linePath,
					fill: fill,
					stroke: stroke,
					"stroke-width": strokeWidth,
					identifier: id
				}
		);
		
		if ( drawArrow )
			me.items.push(
				{
						type: "path",
						path: arrowPath,
						fill: fill,
						rotation:
						{
							angle: 90+angle
						},
						stroke: stroke,
						"stroke-width": strokeWidth,
						identifier: id
				}
			);
	},
	afterRender: function() {
		var me = this;
		var animationTimer = me.animationTimer;
		
		me.callParent(arguments);
		
		if ( animationTimer != 0 )
			clearInterval(animationTimer);
		
		me.surface.items.each(function (shp, i)
			{
				
				if ( shp.selectable )
					shp.addListener("click", function(shp)
					{
						this.viewport = this.viewport || this.up('viewport');
						
						if ( !this.shiftKeyPressed ){
							this.selected = [];
							me.surface.items.each(function(s, i)
							{
								s.setAttributes(
								{
									fill: this.imageItems[i].fill
								}, true);
							}, me);
						}
						
						this.selected.push(shp.identifier);
						shp.setAttributes(
							{
								fill : "yellow"
							}, true);
						
						this.viewport.fireEvent('edit',this.taskId, this.editorId, this.selected );
					}, me);
			}
		);
		
		me.animationTimer = setInterval(function()
		{
			me.surface.items.each(function (shp, i)
			{
				if ( me.items[i].animation )
				{
					shp.flip = !shp.flip;
					shp.animate(
					{
						to:
						{
							opacity: !shp.flip ? 1 : 0
						},
						duration: 800
					}
					);		
				}
			});
		}, 1000);
	},
	apiLoaded: function() {

		this.setupMap();
	},
	apiLoaded: function() {
	
		this.setupMap();
	},
	
    afterComponentLayout: function() {
    	
    	this.callParent(arguments);
    },
	setValue: function(value) {
	},
	selfUpdate: function(def) {
		var me = this;
	
		//Update perspective

		//Update markers
	},
	onDestroy: function() {
		
	}
});
