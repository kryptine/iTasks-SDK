Ext.ns("itasks");

itasks.GMapPanel = Ext.extend( Ext.Panel, {

	initComponent : function(){
		
		Ext.applyIf(this,
			{ url: "/handlers/work/tab"
			, zoom: 15
			, center : [51.824118,5.868174]
			, mapType : "ROADMAP"
            , border: false
			, autoHeight: false
			, height: 400
			, width: 500
			, scope: this
			});

		itasks.GMapPanel.superclass.initComponent.apply(this,arguments);
		
		this.show();
	},
	
	afterRender : function(){
				
		itasks.GMapPanel.superclass.afterRender.call(this);  
		
		var getMapType = function (mapType){
			return eval("google.maps.MapTypeId."+mapType);
		}
		
		var options = 
			{ center : new google.maps.LatLng(this.center[0],this.center[1])
			, zoom: this.zoom
			, mapTypeId : getMapType(this.mapType)
		}
		
		var gmap = new google.maps.Map(this.body.dom, options);

		this.addMarkers(gmap)
		
		var parent = this;
		
		var mvcEventHandler = function(){
			
			var ll = gmap.getCenter();
			var zm = gmap.getZoom();
			
			var value = {
				center : [ll.lat(),ll.lng()],
				zoom   : zm,
				type   : gmap.getMapTypeId().toUpperCase()
			}
			
			var ct = parent.findParentByType("itasks.task-ext-form");
			if(!ct) return;
			
			ct.addUpdate(parent.name, Ext.encode(value));
			ct.sendUpdates();
		}
		
		var lclickEventHandler = function(event){
			
			var ll = event.latLng
			
			var value = {
				event 	: "LEFTCLICK",
				source 	: "MAP",
				point	: [ll.lat(),ll.lng()]
			}
			
			var ct = parent.findParentByType("itasks.task-ext-form");
			if(!ct) return;
			
			ct.addUpdate(parent.name, Ext.encode(value));
			ct.sendUpdates();			
		}
				
		if(this.isEditor){
			google.maps.event.addListener(gmap, 'maptypeid_changed', mvcEventHandler);
			google.maps.event.addListener(gmap, 'idle', mvcEventHandler);
			google.maps.event.addListener(gmap, 'click', lclickEventHandler);
		}
				
	},
	
	addMarkers : function (gmap){
		var i=0;
		for(i=0; i<this.markers.length; i++){
						
			var markerObj = new google.maps.Marker({
				map : gmap,
				position : new google.maps.LatLng(this.markers[i].position[0],this.markers[i].position[1])
			});		
		}		
	}
});

Ext.reg('itasks.gmappanel', itasks.GMapPanel);