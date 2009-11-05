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
	
	setValue : function(_data){
		var data = Ext.decode(_data);
		//this.gmap.setCenter(new google.maps.LatLng(data.center[0],data.center[1]));
		//this.gmap.setZoom(data.zoom);
		//this.gmap.setMapTypeId(this.getMapType(data.mapType));
	},
	
	getMapType : function (mapType){
		return eval("google.maps.MapTypeId."+mapType);
	},

	afterRender : function(){
				
		itasks.GMapPanel.superclass.afterRender.call(this);  
		
		
		var options = 
			{ center : new google.maps.LatLng(this.center[0],this.center[1])
			, zoom: this.zoom
			, mapTypeId : this.getMapType(this.mapType)
		}
		
		this.gmap = new google.maps.Map(this.body.dom, options);

		//this.addMarkers(gmap)
		
		var parent = this;
		
		var mvcEventHandler = function(){
			
			var ll = parent.gmap.getCenter();
			var zm = parent.gmap.getZoom();
			
			var value = {
				center : [ll.lat(),ll.lng()],
				zoom   : zm,
				type   : parent.gmap.getMapTypeId().toUpperCase()
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
			google.maps.event.addListener(this.gmap, 'maptypeid_changed', mvcEventHandler);
			google.maps.event.addListener(this.gmap, 'idle', mvcEventHandler);
			google.maps.event.addListener(this.gmap, 'click', lclickEventHandler);
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